#include <atomic>
#include <exception>
#include <type_traits>

namespace rwx {

class bad_weak_ptr : public std::exception {
public:
    virtual const char* what() const noexcept {
        return "bad_weak_ptr";
    }

    virtual ~bad_weak_ptr() noexcept = default;
};

class SpCountedBase {
public:
    SpCountedBase() noexcept : use_count_(1), weak_count_(1) {}

    virtual ~SpCountedBase() noexcept {}

    /* Called when use_count_ drops to zero, to release the resources managed by *this. */
    virtual void dispose() noexcept = 0;

    /* Called when weak_count_ drops to zero. */
    virtual void destroy() noexcept { delete this; }

    virtual void* get_deleter(const std::type_info&) noexcept = 0;

    void add_ref_copy() { ++use_count_; }

    void add_ref_lock() {
        /* Perform lock-free add-if-not-zero operation. */
        auto count = get_use_count();
        /* Replace the current counter value with the old value + 1, as long as it's not changed meanwhile. */
        do {
            if (count == 0)
                throw bad_weak_ptr();
        } while (not use_count_.compare_exchange_weak(count, count + 1,
                                                      std::memory_order_acq_rel,
                                                      std::memory_order_relaxed));
    }

    bool add_ref_lock_nothrow() {
        /* Perform lock-free add-if-not-zero operation. */
        auto count = get_use_count();
        /* Replace the current counter value with the old value + 1, as long as it's not changed meanwhile. */
        do {
            if (count == 0)
                return false;
        } while (not use_count_.compare_exchange_weak(count, count + 1,
                                                      std::memory_order_acq_rel,
                                                      std::memory_order_relaxed));
        return true;
    }

    void release() noexcept {
        if (use_count_.fetch_sub(1) == 1) {
            dispose();

            if (weak_count_.fetch_sub(1) == 1)
                destroy();
        }
    }

    void weak_add_ref() noexcept { ++weak_count_; }

    void weak_release() noexcept {
        if (weak_count_.fetch_sub(1) == 1)
            destroy();
    }

    long get_use_count() const noexcept {
        /* No memory barrier is used here so there is no synchronization with other threads. */
        return use_count_.load(std::memory_order_relaxed);
    }

private:
    SpCountedBase(const SpCountedBase&) = delete;
    SpCountedBase& operator=(const SpCountedBase&) = delete;

    std::atomic_int use_count_;
    std::atomic_int weak_count_;
};


// Forward declarations.
template <typename T>
class shared_ptr;

template <typename T>
class weak_ptr;

template <typename T>
class enable_shared_from_this;

template <typename T>
class shared_ptr;

template <typename T>
class weak_ptr;

template <typename T>
struct owner_less;

template <typename T>
class enable_shared_from_this;

class weak_count;

class shared_count;


template <typename Ptr>
class SpCountedPtr final : public SpCountedBase {
public:
    explicit SpCountedPtr(Ptr p) noexcept : ptr_(p) {}

    virtual void dispose() noexcept { delete ptr_; }

    virtual void destroy() noexcept { delete this; }

    virtual void* get_deleter(const std::type_info&) noexcept { return nullptr; }

    SpCountedPtr(const SpCountedPtr&) = delete;
    SpCountedPtr& operator=(const SpCountedPtr&) = delete;

private:
    Ptr ptr_;
};

template <>
inline void SpCountedPtr<std::nullptr_t>::dispose() noexcept {}


template <int Nm, typename T, bool use_ebo = not std::is_final(T) and std::is_empty(T)>
struct SpEboHelper;

/// Specialization using EBO.
template <int Nm, typename T>
struct SpEboHelper<Nm, T, true> : private T {
    explicit SpEboHelper(const T& t) : T(t) {}

    static T& get(SpEboHelper& eboh) { return static_cast<T&>(eboh); }
};

/// Specialization not using EBO.
template <int Nm, typename T>
struct SpEboHelper<Nm, T, false> {
    explicit SpEboHelper(const T& t) : t_(t) {}

    static T& get(SpEboHelper& eboh) { return eboh.t_; }

private:
    T t_;
};

// Support for custom deleter and/or allocator
template <typename Ptr, typename Deleter, typename Alloc>
class SpCountedDeleter final : public SpCountedBase {
    class Impl : SpEboHelper<0, Deleter>, SpEboHelper<1, Alloc> {
        using DeleterBase = SpEboHelper<0, Deleter>;
        using AllocBase = SpEboHelper<1, Alloc>;

    public:
        Impl(Ptr p, Deleter d, const Alloc& a) noexcept
            : ptr_(p), DeleterBase(d), AllocBase(a)
        {}

        Deleter& deleter() noexcept { return DeleterBase::get(*this); }
        Alloc& alloc() noexcept { return AllocBase::get(*this); }

        Ptr ptr_;
    };

public:
    // d(p) must not throw
    SpCountedDeleter(Ptr p, Deleter d) noexcept
        : impl_(p, d, Alloc())
    {}

    // d(p) must not throw
    SpCountedDeleter(Ptr p, Deleter d, const Alloc& a) noexcept
        : impl_(p, d, a)
    {}

    ~SpCountedDeleter() noexcept {}

    virtual void dispose() noexcept {
        impl_.deleter()(impl_.ptr_);
    }

    virtual void destroy() noexcept {
        using AllocTraits = typename std::allocator_traits<Alloc>::template std::rebind_traits<SpCountedDeleter>;

        typename AllocTraits::allocator_type a(impl_.alloc());
        AllocTraits::destroy(a, this);
        AllocTraits::deallocate(a, this, 1);
    }

    virtual void* get_deleter(const std::type_info& ti) noexcept {
        return ti == typeid(Deleter)
               ? std::addressof(impl_.del_())
               : nullptr;
    }

private:
    Impl impl_;
};


class shared_count {
public:
    constexpr shared_count() noexcept : pi_(0) {};

    template <typename Ptr>
    explicit shared_count(Ptr p) : pi_(0) {
        try {
            pi_ = new SpCountedPtr<Ptr>(p_);
        } catch (...) {
            delete p;
            throw;
        }
    }

    template <typename Ptr, typename Deleter>
    shared_count(Ptr p, Deleter d) : shared_count(p, std::move(d), std::allocator<void>()) {};

    template <typename Ptr, typename Deleter, typename Alloc>
    shared_count(Ptr p, Deleter d, Alloc a) : pi_(0) {
        using SpCD = SpCountedDeleter<Ptr, Deleter, Alloc>;
        using AllocTraits = typename std::allocator_traits<Alloc>::template std::rebind_traits<SpCD>;

        typename AllocTraits::allocator_type a2(a);
        SpCD* mem{};
        try {
            mem = AllocTraits::allocate(a2, 1);
            AllocTraits::construct(a2, mem, p, std::move(d), std::move(a));
            pi_ = mem;
        } catch (...) {
            d(p);  // Call Deleter on p
            if (mem)
                AllocTraits::deallocate(a2, mem, 1);
            throw;
        }
    };

    // TODO:
    // TODO:

    // Throw bad_weak_ptr when r.get_use_count() == 0.
    explicit shared_count(const weak_count& r);

    // Does not throw if r.get_use_count() == 0, caller must check.
    explicit shared_count(const weak_count& r, std::nothrow_t);

    ~shared_count() noexcept {
        if (pi_)
            pi_->release();
    }

    shared_count(const shared_count& r) noexcept : pi_(r.pi_) {
        if (pi_)
            pi_->add_ref_copy();
    }

    shared_count& operator=(const shared_count& r) noexcept {
        SpCountedBase* tmp = r.pi_;
        if (tmp != pi_) {
            if (tmp)
                tmp->add_ref_copy();
            if (pi_)
                pi_->release();
            pi_ = tmp;
        }
        return *this;
    }

    void swap(shared_count& r) noexcept {
        SpCountedBase* tmp = r.pi_;
        r.pi_ = pi_;
        pi_ = tmp;
    }

    long get_use_count() const noexcept {
        return pi_ ? pi_->get_use_count() : 0;
    }

    bool unique() const noexcept {
        return get_use_count() == 1;
    }

    void* get_deleter(const std::type_info& ti) const noexcept {
        return pi_ ? pi_->get_deleter(ti) : nullptr;
    }

    bool less(const shared_count& r) const noexcept {
        return std::less<SpCountedBase*>()(pi_, r.pi_);
    }

    bool less(const weak_count& r) const noexcept {
        return std::less<SpCountedBase*>()(pi_, r.pi_);
    }

    friend inline bool operator==(const shared_count& a, const shared_count& b) noexcept {
        return a.pi_ == b.pi_;
    }

private:
    friend class weak_count;

private:
    SpCountedBase* pi_;
};


class weak_count {
public:
    constexpr weak_count() noexcept : pi_(0) {}

    weak_count(const shared_count& r) noexcept : pi_(r.pi_) {
        if (pi_)
            pi_->weak_add_ref();
    }

    weak_count(const weak_count& r) noexcept : pi_(r.pi_) {
        if (pi_)
            pi_->weak_add_ref();
    }

    weak_count(weak_count&& r) noexcept : pi_(r.pi_) {
        r.pi_ = nullptr;
    }

    ~weak_count() noexcept {
        if (pi_)
            pi_->weak_release();
    }

    weak_count& operator=(const shared_count& r) noexcept {
        SpCountedBase* tmp = r.pi_;
        if (tmp)
            tmp.weak_add_ref();
        if (pi_)
            pi_->weak_release();
        pi_ = tmp;
        return *this;
    }

    weak_count& operator=(const weak_count& r) noexcept {
        SpCountedBase* tmp = r.pi_;
        if (tmp)
            tmp->weak_add_ref();
        if (pi_)
            pi->weak_release();
        pi_ = tmp;
        return *this;
    }

    weak_count& operator=(weak_count&& r) noexcept {
        if (pi_)
            pi_->weak_release();
        pi_ = r.pi_;
        r.pi_ = nullptr;
        return *this;
    }

    void swap(weak_count& r) noexcept {
        SpCountedBase* tmp = r.pi_;
        r.pi_ = pi_;
        pi_ = tmp;
    }

    long get_use_count() const noexcept {
        return pi_ ? pi_->get_use_count() : 0;
    }

    bool less(const weak_count& r) const noexcept {
        return std::less<SpCountedBase*>()(pi_, r.pi_);
    }

    bool less(const shared_count& r) const noexcept {
        return std::less<SpCountedBase*>()(pi_, r.pi);
    }

    // Friend functioin injected into enclosing namespace and founc by ADL
    friend inline bool operator==(const weak_count& a, const weak_count& b) noexcept {
        return a.pi_ == b.pi_;
    }

private:
    friend class shared_count;

    SpCountedBase* pi_;
};


/* A smart pointer with reference-counted copy semantics.
 *
 * The object pointed to is deleted when the last shared_ptr pointing to
 * it is destroyed or reset.
 */
template <typename T>
class shared_ptr {
    template <typename Ptr>
    using Convertible = typename std::enable_if<std::is_convertible<Ptr, T*>::value>::type;

public:
    using element_type = T;

private:
    T* ptr_;  // Contained pointer
    shared_count refcount_;  // Reference counter
};


}
