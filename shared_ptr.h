#include <atomic>
#include <exception>
#include <type_traits>
#include <memory>
#include <functional>

namespace rwx {

using std::nullptr_t;

class bad_weak_ptr : public std::exception {
public:
    virtual const char* what() const noexcept {
        return "bad_weak_ptr";
    }

    virtual ~bad_weak_ptr() noexcept = default;
};

class _sp_counted_base {
public:
    _sp_counted_base() noexcept : use_count_(1), weak_count_(1) {}

    virtual ~_sp_counted_base() noexcept = default;

    /* Called when use_count_ drops to zero, to release the resources managed by *this. */
    virtual void dispose() noexcept = 0;

    /* Called when weak_count_ drops to zero. */
    virtual void destroy() noexcept { delete this; }

    virtual void* get_deleter(const std::type_info&) noexcept = 0;

    void add_ref_copy() {
        use_count_.fetch_add(1, std::memory_order_acq_rel);
    }

    /* Perform lock-free add-if-equal operation. */
    void add_ref_lock() {
        auto count = get_use_count();
        /* Replace the current counter value with the old value + 1, as long as it's not changed meanwhile. */
        do {
            if (count == 0)
                throw bad_weak_ptr();
        } while (not use_count_.compare_exchange_weak(count, count + 1,
                                                      std::memory_order_acq_rel,
                                                      std::memory_order_relaxed));
    }

    /* Perform lock-free add-if-equal operation. */
    bool add_ref_lock_nothrow() {
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
        if (use_count_.fetch_sub(1, std::memory_order_acq_rel) == 1) {
            dispose();

            if (weak_count_.fetch_sub(1) == 1)
                destroy();
        }
    }

    void weak_add_ref() noexcept {
        weak_count_.fetch_add(1, std::memory_order_acq_rel);
    }

    void weak_release() noexcept {
        if (weak_count_.fetch_sub(1, std::memory_order_acq_rel) == 1)
            destroy();
    }

    long get_use_count() const noexcept {
        /* No memory barrier is used here so there is no synchronization with other threads. */
        return use_count_.load(std::memory_order_relaxed);
    }

private:
    // No copy.
    _sp_counted_base(const _sp_counted_base&) = delete;
    _sp_counted_base& operator=(const _sp_counted_base&) = delete;

    std::atomic_long use_count_;
    std::atomic_long weak_count_;
};

// Forward declarations.
template <typename T>
class shared_ptr;

template <typename T>
class weak_ptr;

template <typename T>
class enable_shared_from_this;

class weak_count;

class shared_count;

template <typename Ptr>
class _sp_counted_ptr final : public _sp_counted_base {
public:
    explicit _sp_counted_ptr(Ptr p) noexcept : ptr_(p) {}

    virtual void dispose() noexcept { delete ptr_; }

    virtual void destroy() noexcept { delete this; }

    virtual void* get_deleter(const std::type_info&) noexcept { return nullptr; }

    // No copy
    _sp_counted_ptr(const _sp_counted_ptr&) = delete;
    _sp_counted_ptr& operator=(const _sp_counted_ptr&) = delete;

private:
    Ptr ptr_;
};

// Specialization for null pointer.
template <>
inline void _sp_counted_ptr<std::nullptr_t>::dispose() noexcept {}

template <int Nm, typename T, bool use_ebo = not std::is_final<T>::value and std::is_empty<T>::value>
struct _sp_ebo_helper;

// Specialization using EBO.
template <int Nm, typename T>
struct _sp_ebo_helper<Nm, T, true> : private T {
    explicit _sp_ebo_helper(const T& t) : T(t) {}

    static T& get(_sp_ebo_helper& eboh) { return static_cast<T&>(eboh); }
};

// Specialization not using EBO.
template <int Nm, typename T>
struct _sp_ebo_helper<Nm, T, false> {
    explicit _sp_ebo_helper(const T& t) : t_(t) {}

    static T& get(_sp_ebo_helper& eboh) { return eboh.t_; }

private:
    T t_;
};

// Support for custom deleter and/or allocator
template <typename Ptr, typename Deleter, typename Alloc>
class _sp_counted_deleter final : public _sp_counted_base {
    class Impl : _sp_ebo_helper<0, Deleter>, _sp_ebo_helper<1, Alloc> {
        using DeleterBase = _sp_ebo_helper<0, Deleter>;
        using AllocBase = _sp_ebo_helper<1, Alloc>;

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
    _sp_counted_deleter(Ptr p, Deleter d) noexcept
        : impl_(p, d, Alloc())
    {}

    // d(p) must not throw
    _sp_counted_deleter(Ptr p, Deleter d, const Alloc& a) noexcept
        : impl_(p, d, a)
    {}

    virtual void dispose() noexcept {
        impl_.deleter()(impl_.ptr_);
    }

    virtual void destroy() noexcept {
        using AllocTraits = typename std::allocator_traits<Alloc>::template rebind_traits<_sp_counted_deleter>;

        typename AllocTraits::allocator_type a(impl_.alloc());
        AllocTraits::destroy(a, this);
        AllocTraits::deallocate(a, this, 1);
    }

    virtual void* get_deleter(const std::type_info& ti) noexcept {
        return ti == typeid(Deleter)
               ? std::addressof(impl_.deleter())
               : nullptr;
    }

private:
    Impl impl_;
};

template <typename T>
struct _aligned_buffer
    : std::aligned_storage<sizeof(T), std::alignment_of<T>::value>
{
    typename std::aligned_storage<sizeof(T), std::alignment_of<T>::value>::type storage_;

    void* addr() noexcept {
        return static_cast<void*>(&storage_);
    }

    const void* addr() const noexcept {
        return static_cast<const void*>(&storage_);
    }

    T* ptr() noexcept {
        return static_cast<T*>(addr());
    }

    const T* ptr() const noexcept {
        return static_cast<const T*>(addr());
    }
};

struct _sp_make_shared_tag {};

template <typename T, typename Alloc>
class _sp_counted_ptr_inplace final : public _sp_counted_base {
    class Impl : _sp_ebo_helper<0, Alloc> {
        using AllocBase = _sp_ebo_helper<0, Alloc>;

    public:
        explicit Impl(Alloc a) noexcept : AllocBase(a) {}

        Alloc& alloc() noexcept { return AllocBase::get(*this); }

        _aligned_buffer<T> storage_;
    };

public:
    template <typename... Args>
    _sp_counted_ptr_inplace(Alloc a, Args&&... args) : impl_(a) {
        // allocate_shared should use allocator_traits<A>::construct
        std::allocator_traits<Alloc>::construct(a, ptr(),
                                                std::forward<Args>(args)...);  // might throw
    }

    ~_sp_counted_ptr_inplace() noexcept {}

    virtual void dispose() noexcept {
        std::allocator_traits<Alloc>::destroy(impl_.alloc(), ptr());
    }

    // Override because the allocator needs to know the dynamic type
    virtual void destroy() noexcept {
        using AllocTraits = typename std::allocator_traits<Alloc>::template rebind_traits<_sp_counted_ptr_inplace>;
        typename AllocTraits::allocator_type a(impl_.alloc());
        AllocTraits::destroy(a, this);
        AllocTraits::deallocate(a, this, 1);
    }

    // Sneaky trick so shared_ptr can get the managed pointer
    virtual void* get_deleter(const std::type_info& ti) noexcept {
        if (ti == typeid(_sp_make_shared_tag))
            return const_cast<typename std::remove_cv<T>::type*>(ptr());
        return nullptr;
    }
private:
    T* ptr() noexcept { return impl_.storage_.ptr(); }

    Impl impl_;
};

class shared_count {
public:
    constexpr shared_count() noexcept : pi_(0) {};

    template <typename Ptr>
    explicit shared_count(Ptr p) : pi_(0) {
        try {
            pi_ = new _sp_counted_ptr<Ptr>(p);
        } catch (...) {
            delete p;
            throw;
        }
    }

    template <typename Ptr, typename Deleter>
    shared_count(Ptr p, Deleter d) : shared_count(p, std::move(d), std::allocator<void>()) {};

    template <typename Ptr, typename Deleter, typename Alloc>
    shared_count(Ptr p, Deleter d, Alloc a) : pi_(0) {
        using SpCD = _sp_counted_deleter<Ptr, Deleter, Alloc>;
        using AllocTraits = typename std::allocator_traits<Alloc>::template rebind_traits<SpCD>;

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

    template <typename T, typename Alloc, typename... Args>
    shared_count(_sp_make_shared_tag, T*, const Alloc& a, Args&&... args) : pi_(0) {
        using _sp_cp_type = _sp_counted_ptr_inplace<T, Alloc>;
        using AllocTraits = typename std::allocator_traits<Alloc>::template rebind_traits<_sp_cp_type>;
        typename AllocTraits::allocator_type a2(a);
        _sp_cp_type* mem = AllocTraits::allocate(a2, 1);
        try {
            AllocTraits::construct(a2, mem, std::move(a),
                                   std::forward<Args>(args)...);
            pi_ = mem;
        } catch (...) {
            AllocTraits::deallocate(a2, mem, 1);
            throw;
        }
    };

    // Special case for std::unique_ptr<T, Deleter> to provide the strong guarantee.
    template <typename T, typename Deleter>
    explicit shared_count(std::unique_ptr<T, Deleter>&& r) : pi_(0) {
        // Inconsistency between unique_ptr and shared_ptr
        if (r.get() == nullptr)
            return;

        using Ptr = typename std::unique_ptr<T, Deleter>::pointer;
        using Deleter2 = typename std::conditional<std::is_reference<Deleter>::value,
                                                    std::reference_wrapper<typename std::remove_reference<Deleter>::type>,
                                                    Deleter>::type;
        using _sp_cd_type = _sp_counted_deleter<Ptr, Deleter2, std::allocator<void>>;
        using Alloc = std::allocator<_sp_cd_type>;
        using AllocTraits = std::allocator_traits<Alloc>;
        Alloc a;
        _sp_cd_type* mem = AllocTraits::allocate(a, 1);
        AllocTraits::construct(a, mem, r.release(),
                               r.get_deleter());  // non-throwing
        pi_ = mem;
    };

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
        _sp_counted_base* tmp = r.pi_;
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
        _sp_counted_base* tmp = r.pi_;
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
        return std::less<_sp_counted_base*>()(pi_, r.pi_);
    }

    bool less(const weak_count& r) const noexcept;

    friend inline bool operator==(const shared_count& a, const shared_count& b) noexcept {
        return a.pi_ == b.pi_;
    }

private:
    friend class weak_count;

    _sp_counted_base* pi_;
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
        _sp_counted_base* tmp = r.pi_;
        if (tmp)
            tmp->weak_add_ref();
        if (pi_)
            pi_->weak_release();
        pi_ = tmp;
        return *this;
    }

    weak_count& operator=(const weak_count& r) noexcept {
        _sp_counted_base* tmp = r.pi_;
        if (tmp)
            tmp->weak_add_ref();
        if (pi_)
            pi_->weak_release();
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
        _sp_counted_base* tmp = r.pi_;
        r.pi_ = pi_;
        pi_ = tmp;
    }

    long get_use_count() const noexcept {
        return pi_ ? pi_->get_use_count() : 0;
    }

    bool less(const weak_count& r) const noexcept {
        return std::less<_sp_counted_base*>()(pi_, r.pi_);
    }

    bool less(const shared_count& r) const noexcept {
        return std::less<_sp_counted_base*>()(pi_, r.pi_);
    }

    friend inline bool operator==(const weak_count& a, const weak_count& b) noexcept {
        return a.pi_ == b.pi_;
    }

private:
    friend class shared_count;

    _sp_counted_base* pi_;
};

// Now that weak_count is defined we can define this constructor:
inline shared_count::shared_count(const weak_count& r)
    : pi_(r.pi_)
{
    if (pi_)
        pi_->add_ref_lock();
    else
        throw bad_weak_ptr();
}

// Now that weak_count is defined we can define this constructor:
inline shared_count::shared_count(const weak_count& r, std::nothrow_t)
    : pi_(r.pi_)
{
    if (pi_ and not pi_->add_ref_lock_nothrow())
        pi_ = nullptr;
}

inline bool shared_count::less(const weak_count& r) const noexcept {
    return std::less<_sp_counted_base*>()(pi_, r.pi_);
}

// Support for enable_shared_from_this.

// Friend of enable_shared_from_this.
// Match object type that inherit from enable_shared_from_this.
template <typename T1, typename T2>
void enable_shared_from_this_helper(const shared_count&,
                                    const enable_shared_from_this<T1>*,
                                    const T2*) noexcept;

// Match any other type that doesn't inherit from enable_shared_from_this.
inline void enable_shared_from_this_helper(const shared_count&, ...) noexcept {}

/**
 *  @brief  A smart pointer with reference-counted copy semantics.
 *
 *  The object pointed to is deleted when the last shared_ptr pointing to
 *  it is destroyed or reset.
*/
template <typename T>
class shared_ptr {
    template <typename Ptr>
    using Convertible = typename std::enable_if<std::is_convertible<Ptr, T*>::value>::type;

public:
    using element_type = T;

    /**
     *  @brief  Construct an empty %shared_ptr.
     *  @post   use_count()==0 && get()==0
     */
    constexpr shared_ptr() noexcept : ptr_(nullptr), refcount_() {}

    shared_ptr(const shared_ptr&) noexcept = default;

    ~shared_ptr() = default;

    /**
     *  @brief  Construct a %shared_ptr that owns the pointer @a p.
     *  @param  p  A pointer that is convertible to element_type*.
     *  @post   use_count() == 1 && get() == p
     *  @throw  std::bad_alloc, in which case @c delete @a p is called.
     */
    template <typename T1>
    explicit shared_ptr(T1* p)
        : ptr_(p), refcount_(p)
    {
        static_assert(not std::is_void<T1>::value, "incomplete type");
        static_assert(sizeof(T1) > 0, "incomplete type");
        enable_shared_from_this_helper(refcount_, p, p);
    }

    /**
     *  @brief  Construct a %shared_ptr that owns the pointer @a p
     *          and the deleter @a d.
     *  @param  p  A pointer.
     *  @param  d  A deleter.
     *  @post   use_count() == 1 && get() == p
     *  @throw  std::bad_alloc, in which case @a d(p) is called.
     *
     *  Requirements: Deleter's copy constructor and destructor must
     *  not throw
     *
     *  shared_ptr will release p by calling d(p)
     */
    template <typename T1, typename Deleter>
    shared_ptr(T1* p, Deleter d)
        : ptr_(p), refcount_(p, std::move(d))
    {
        enable_shared_from_this_helper(refcount_, p, p);
    }

    /**
     *  @brief  Construct a %shared_ptr that owns the pointer @a p
     *          and the deleter @a d.
     *  @param  p  A pointer.
     *  @param  d  A deleter.
     *  @param  a  An allocator.
     *  @post   use_count() == 1 && get() == p
     *  @throw  std::bad_alloc, in which case @a d(p) is called.
     *
     *  Requirements: Deleter's copy constructor and destructor must
     *  not throw, Alloc's copy constructor and destructor must not
     *  throw.
     *
     *  shared_ptr will release p by calling d(p)
     */
    template <typename T1, typename Deleter, typename Alloc>
    shared_ptr(T1* p, Deleter d, Alloc a)
        : ptr_(p), refcount_(p, std::move(d), std::move(a))
    {
        enable_shared_from_this_helper(refcount_, p, p);
    }

    /**
     *  @brief  Construct a %shared_ptr that owns a null pointer
     *          and the deleter @a d.
     *  @param  p  A null pointer constant.
     *  @param  d  A deleter.
     *  @post   use_count() == 1 && get() == p
     *  @throw  std::bad_alloc, in which case @a d(p) is called.
     *
     *  Requirements: Deleter's copy constructor and destructor must
     *  not throw
     *
     *  The last owner will call d(p)
     */
    template <typename Deleter>
    shared_ptr(nullptr_t p, Deleter d)
        : ptr_(0), refcount_(p, std::move(d))
    {}

    /**
     *  @brief  Construct a %shared_ptr that owns a null pointer
     *          and the deleter @a d.
     *  @param  p  A null pointer constant.
     *  @param  d  A deleter.
     *  @param  a  An allocator.
     *  @post   use_count() == 1 && get() == p
     *  @throw  std::bad_alloc, in which case @a d(p) is called.
     *
     *  Requirements: Deleter's copy constructor and destructor must
     *  not throw Alloc's copy constructor and destructor must not
     *  throw.
     *
     *  The last owner will call d(p)
     */
    template <typename Deleter, typename Alloc>
    shared_ptr(nullptr_t p, Deleter d, Alloc a)
        : ptr_(0), refcount_(p, std::move(d), std::move(a))
    {}

    // Aliasing constructor

    /**
     *  @brief  Constructs a %shared_ptr instance that stores @a p
     *          and shares ownership with @a r.
     *  @param  r  A %shared_ptr.
     *  @param  p  A pointer that will remain valid while @a *r is valid.
     *  @post   get() == p && use_count() == r.use_count()
     *
     *  This can be used to construct a @c shared_ptr to a sub-object
     *  of an object managed by an existing @c shared_ptr.
     *
     * @code
     * shared_ptr< pair<int,int> > pii(new pair<int,int>());
     * shared_ptr<int> pi(pii, &pii->first);
     * assert(pii.use_count() == 2);
     * @endcode
     */
    template <typename T1>
    shared_ptr(const shared_ptr<T1>& r, T* p) noexcept
        : ptr_(p), refcount_(r.refcount_)
    {}

    /**
     *  @brief  If @a r is empty, constructs an empty %shared_ptr;
     *          otherwise construct a %shared_ptr that shares ownership
     *          with @a r.
     *  @param  r  A %shared_ptr.
     *  @post   get() == r.get() && use_count() == r.use_count()
     */
    template <typename T1, typename = Convertible<T1*>>
    shared_ptr(const shared_ptr<T1>& r) noexcept
        : ptr_(r.ptr_), refcount_(r.refcount_)
    {}

    /**
     *  @brief  Move-constructs a %shared_ptr instance from @a r.
     *  @param  r  A %shared_ptr rvalue.
     *  @post   *this contains the old value of @a r, @a r is empty.
     */
    shared_ptr(shared_ptr&& r) noexcept
        : ptr_(r.ptr_), refcount_()
    {
        refcount_.swap(r.refcount_);
        r.ptr_ = 0;
    }

    /**
     *  @brief  Move-constructs a %shared_ptr instance from @a r.
     *  @param  r  A %shared_ptr rvalue.
     *  @post   *this contains the old value of @a r, @a r is empty.
     */
    template <typename T1, typename = Convertible<T1*>>
    shared_ptr(shared_ptr<T1>&& r) noexcept
        : ptr_(r.ptr_), refcount_()
    {
        refcount_.swap(r.refcount_);
        r.ptr_ = 0;
    }

    /**
     *  @brief  Constructs a %shared_ptr that shares ownership with @a r
     *          and stores a copy of the pointer stored in @a r.
     *  @param  r  A weak_ptr.
     *  @post   use_count() == r.use_count()
     *  @throw  bad_weak_ptr when r.expired(),
     *          in which case the constructor has no effect.
     */
    template <typename T1>
    explicit shared_ptr(const weak_ptr<T1>& r)
        : refcount_(r.refcount_)  // may throw
    {
        // It is now safe to copy r.ptr_, as
        // refcount_(r.refcount_) did not throw.
        ptr_ = r.ptr_;
    }

    // shared_ptr's constructor from unique_ptr should be constrained
    // If an exception is thrown this constructor has no effect.
    template <typename T1, typename Deleter,
               typename = Convertible<typename std::unique_ptr<T1, Deleter>::pointer>>
    shared_ptr(std::unique_ptr<T1, Deleter>&& r)
        : ptr_(r.get()), refcount_() {
        auto raw = raw_ptr(r.get());
        refcount_ = shared_count(std::move(r));
        enable_shared_from_this_helper(refcount_, raw, raw);
    }

    /**
     *  @brief  Construct an empty %shared_ptr.
     *  @post   use_count() == 0 && get() == nullptr
     */
    constexpr shared_ptr(nullptr_t) noexcept : shared_ptr() {}

    shared_ptr& operator=(const shared_ptr&) noexcept = default;

    template <typename T1>
    shared_ptr& operator=(const shared_ptr<T1>& r) noexcept {
        ptr_ = r.ptr_;
        refcount_ = r.refcount_;  // shared_count::operator== doesn't throw
        return *this;
    }

    shared_ptr& operator=(shared_ptr&& r) noexcept {
        shared_ptr(std::move(r)).swap(*this);
        return *this;
    }

    template <typename T1>
    shared_ptr& operator=(shared_ptr<T1>&& r) noexcept {
        shared_ptr(std::move(r)).swap(*this);
        return *this;
    }

    template <typename T1, typename Deleter>
    shared_ptr& operator=(std::unique_ptr<T1, Deleter>&& r) {
        shared_ptr(std::move(r)).swap(*this);
        return *this;
    };

    void reset() noexcept {
        shared_ptr().swap(*this);
    }

    template <typename T1>
    void reset(T1* p) {  // T1 must be complete
        // Catch self-reset errors
        // assert(p == 0 or p != ptr_);
        shared_ptr(p).swap(*this);
    }

    template <typename T1, typename Deleter>
    void reset(T1* p, Deleter d) {
        shared_ptr(p, d).swap(*this);
    };

    template <typename T1, typename Deleter, typename Alloc>
    void reset(T1* p, Deleter d, Alloc a) {
        shared_ptr(p, d, std::move(a)).swap(*this);
    };

    // Allow class instantiation when T is [cv-qual] void.
    typename std::add_lvalue_reference<T>::type
    operator*() const noexcept {
        // assert(ptr_ != 0);
        return *ptr_;
    }

    T* operator->() const noexcept {
        // assert(ptr_ != 0);
        return ptr_;
    }

    T* get() const noexcept { return ptr_; }

    explicit operator bool() const noexcept {
        return ptr_ != 0;
    }

    bool unique() const noexcept {
        return refcount_.unique();
    }

    long use_count() const noexcept {
        return refcount_.get_use_count();
    }

    void swap(shared_ptr<T>& other) noexcept {
        std::swap(ptr_, other.ptr_);
        refcount_.swap(other.refcount_);
    }

    template <typename T1>
    bool owner_before(const shared_ptr<T1>& rhs) const {
        return refcount_.less(rhs.refcount_);
    }

    template <typename T1>
    bool owner_before(const weak_ptr<T1>& rhs) const {
        return refcount_.less(rhs.refcount_);
    }

protected:
    // This constructor is non-standard, it is used by allocate_shared.
    template <typename Alloc, typename... Args>
    shared_ptr(_sp_make_shared_tag tag, const Alloc& a, Args&&... args)
        : ptr_(), refcount_(tag, (T*)0, a, std::forward<Args>(args)...)
    {
        // ptr_ needs to point to the newly constructed object.
        // This relies on _sp_counted_ptrInplace::get_deleter;
        void* p = refcount_.get_deleter(typeid(tag));
        ptr_ = static_cast<T*>(p);
        enable_shared_from_this_helper(refcount_, ptr_, ptr_);
    }

    template <typename T1, typename Alloc, typename... Args>
    friend shared_ptr<T1> allocate_shared(const Alloc& a, Args&&... args);

    // This constructor is used by weak_ptr::lock().
    shared_ptr(const weak_ptr<T>& r, std::nothrow_t)
        : refcount_(r.refcount_, std::nothrow) {
        ptr_ = refcount_.get_use_count() ? r.ptr_ : nullptr;
    }

    friend class weak_ptr<T>;

private:
    void* get_deleter(const std::type_info& ti) const noexcept {
        return refcount_.get_deleter(ti);
    }

    template <typename T1>
    static T1* raw_ptr(T1* ptr) { return ptr; }

    template <typename T1>
    static auto raw_ptr(T1 ptr) -> decltype(std::addressof(*ptr)) {
        return std::addressof(*ptr);
    }

    template <typename T1> friend class shared_ptr;
    template <typename T1> friend class weak_ptr;

    template <typename Deleter, typename T1>
    friend Deleter* get_deleter(const shared_ptr<T1>&) noexcept;

    T* ptr_;  // Contained pointer
    shared_count refcount_;  // Reference counter
};

// shared_ptr comparisons
template <typename T1, typename T2>
inline bool operator==(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    return a.get() == b.get();
};

template <typename T>
inline bool operator==(const shared_ptr<T>& a, nullptr_t) noexcept {
    return !a;
};

template <typename T>
inline bool operator==(nullptr_t, const shared_ptr<T>& a) noexcept {
    return !a;
};

template <typename T1, typename T2>
inline bool operator!=(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    return a.get() != b.get();
};

template <typename T>
inline bool operator!=(const shared_ptr<T>& a, nullptr_t) noexcept {
    return (bool)a;
}

template <typename T>
inline bool operator!=(nullptr_t, const shared_ptr<T>& a) noexcept {
    return (bool)a;
}

template <typename T1, typename T2>
inline bool operator<(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    using CT = typename std::common_type<T1*, T2*>::type;
    return std::less<CT>()(a.get(), b.get());
};

template <typename T>
inline bool operator<(const shared_ptr<T>& a, nullptr_t) noexcept {
    return std::less<T*>()(a.get(), nullptr);
}

template <typename T>
inline bool operator<(nullptr_t, const shared_ptr<T>& a) noexcept {
    return std::less<T*>()(nullptr, a.get());
}

template <typename T1, typename T2>
inline bool operator<=(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    return !(b < a);
};

template <typename T>
inline bool operator<=(const shared_ptr<T>& a, nullptr_t) noexcept {
    return !(nullptr < a);
}

template <typename T>
inline bool operator<=(nullptr_t, const shared_ptr<T>& a) noexcept {
    return !(a < nullptr);
}

template <typename T1, typename T2>
inline bool operator>(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    return b < a;
};

template <typename T>
inline bool operator>(const shared_ptr<T>& a, nullptr_t) noexcept {
    return std::less<T*>()(nullptr, a.get());
}

template <typename T>
inline bool operator>(nullptr_t, const shared_ptr<T>& a) noexcept {
    return std::less<T*>()(a.get(), nullptr);
}

template <typename T1, typename T2>
inline bool operator>=(const shared_ptr<T1>& a, const shared_ptr<T2>& b) noexcept {
    return !(a < b);
};

template <typename T>
inline bool operator>=(const shared_ptr<T>& a, nullptr_t) noexcept {
    return !(a < nullptr);
}

template <typename T>
inline bool operator>=(nullptr_t, const shared_ptr<T>& a) noexcept {
    return !(nullptr < a);
}

// shared_ptr casts

// The seemingly equivalent code:
// shared_ptr<T>(static_cast<T*>(r.get()))
// will eventually result in undefined behaviour, attempting to
// delete the same object twice.
/// static_pointer_cast
template <typename T, typename T1>
inline shared_ptr<T> static_pointer_cast(const shared_ptr<T1>& r) noexcept {
    return shared_ptr<T>(r, static_cast<T*>(r.get()));
};

// The seemingly equivalent code:
// shared_ptr<T>(const_cast<T*>(r.get()))
// will eventually result in undefined behaviour, attempting to
// delete the same object twice.
/// const_pointer_cast
template <typename T, typename T1>
inline shared_ptr<T> const_pointer_cast(const shared_ptr<T1>& r) noexcept {
    return shared_ptr<T>(r, const_cast<T*>(r.get()));
};

// The seemingly equivalent code:
// shared_ptr<T>(dynamic_cast<T*>(r.get()))
// will eventually result in undefined behaviour, attempting to
// delete the same object twice.
/// dynamic_pointer_cast
template <typename T, typename T1>
inline shared_ptr<T> dynamic_pointer_cast(const shared_ptr<T1>& r) noexcept {
    if (T* p = dynamic_cast<T*>(r.get()))
        return shared_ptr<T>(r, p);
    return shared_ptr<T>();
};

/**
 *  @brief  A smart pointer with weak semantics.
 *
 *  With forwarding constructors and assignment operators.
 */
template <typename T>
class weak_ptr {
    template <typename Ptr>
    using Convertible = typename std::enable_if<std::is_convertible<Ptr, T*>::value>::type;

public:
    using element_type = T;

    constexpr weak_ptr() noexcept : ptr_(nullptr), refcount_() {}

    weak_ptr(const weak_ptr&) noexcept = default;

    ~weak_ptr() = default;

    // The "obvious" converting constructor implementation:
    //
    //  template<typename T1>
    //    weak_ptr(const weak_ptr<T1>& r)
    //    : ptr_(r.ptr_), refcount_(r.refcount_) // never throws
    //    {}
    //
    // has a serious problem.
    //
    //  r.ptr_ may already have been invalidated. The ptr_(r.ptr_)
    //  conversion may require access to *r.ptr_ (virtual inheritance).
    //
    // It is not possible to avoid spurious access violations since
    // in multithreaded programs r.ptr_ may be invalidated at any point.
    template <typename T1, typename = Convertible<T1*>>
    weak_ptr(const weak_ptr<T1>& r) noexcept : refcount_(r.refcount_) {
        ptr_ = r.lock().get();
    };

    template <typename T1, typename = Convertible<T1*>>
    weak_ptr(const shared_ptr<T1>& r) noexcept : ptr_(r.ptr_), refcount_(r.refcount_) {}

    weak_ptr(weak_ptr&& r) noexcept : ptr_(r.ptr_), refcount_(std::move(r.refcount_)) {
        r.ptr_ = nullptr;
    }

    template <typename T1, typename = Convertible<T1*>>
    weak_ptr(weak_ptr<T1>&& r) noexcept : ptr_(r.lock().get()), refcount_(std::move(r.refcount_)) {
        r.ptr_ = nullptr;
    };

    weak_ptr& operator=(const weak_ptr& r) noexcept = default;

    template <typename T1>
    weak_ptr& operator=(const weak_ptr<T1>& r) noexcept {
        ptr_ = r.lock().get();
        refcount_ = r.refcount_;
        return *this;
    }

    template <typename T1>
    weak_ptr& operator=(const shared_ptr<T1>& r) noexcept {
        ptr_ = r.ptr_;
        refcount_ = r.refcount_;
        return *this;
    }

    weak_ptr& operator=(weak_ptr&& r) noexcept {
        ptr_ = r.ptr_;
        refcount_ = std::move(r.refcount_);
        r.ptr_ = nullptr;
        return *this;
    }

    template <typename T1>
    weak_ptr& operator=(weak_ptr<T1>&& r) noexcept {
        ptr_ = r.lock().get();
        refcount_ = std::move(r.refcount_);
        r.ptr_ = nullptr;
        return *this;
    }

    shared_ptr<T> lock() const noexcept {
        return shared_ptr<element_type>(*this, std::nothrow);
    }

    long use_count() const noexcept {
        return refcount_.get_use_count();
    }

    bool expired() const noexcept {
        return refcount_.get_use_count() == 0;
    }

    template <typename T1>
    bool owner_before(const shared_ptr<T1>& rhs) const {
        return refcount_.less(rhs.refcount_);
    }

    template <typename T1>
    bool owner_before(const weak_ptr<T1>& rhs) const {
        return refcount_.less(rhs.refcount_);
    }

    void reset() noexcept { weak_ptr().swap(*this); }

    void swap(weak_ptr& s) noexcept {
        std::swap(ptr_, s.ptr_);
        refcount_.swap(s.refcount_);
    }

private:
    // Used by enable_shared_from_this.
    void assign(T* ptr, const shared_count& refcount) noexcept {
        if (use_count() == 0) {
            ptr_ = ptr;
            refcount_ = refcount;
        }
    }

    template <typename T1> friend class shared_ptr;
    template <typename T1> friend class weak_ptr;
    friend class enable_shared_from_this<T>;

    T* ptr_;  // Contained pointer
    weak_count refcount_;  // Reference counter
};

template <typename T, typename T1>
struct _sp_owner_less : public std::binary_function<T, T, bool> {
    bool operator()(const T& lhs, const T& rhs) const {
        return lhs.owner_before(rhs);
    }

    bool operator()(const T& lhs, const T1& rhs) const {
        return lhs.owner_before(rhs);
    }

    bool operator()(const T1& lhs, const T& rhs) const {
        return lhs.owner_before(rhs);
    }
};

// Primary template owner_less
template <typename T>
struct owner_less;

// Partial specialization of owner_less for shared_ptr.
template <typename T>
struct owner_less<shared_ptr<T>> : public _sp_owner_less<shared_ptr<T>, weak_ptr<T>>
{};

// Partial specialization of owner_less for weak_ptr.
template <typename T>
struct owner_less<weak_ptr<T>> : public _sp_owner_less<weak_ptr<T>, shared_ptr<T>>
{};

/**
 *  @brief Base class allowing use of member function shared_from_this.
 */
template <typename T>
class enable_shared_from_this {
protected:
    constexpr enable_shared_from_this() noexcept {}

    enable_shared_from_this(const enable_shared_from_this&) noexcept {}

    enable_shared_from_this& operator=(const enable_shared_from_this&) noexcept {
        return *this;
    }

    ~enable_shared_from_this() {}

public:
    shared_ptr<T> shared_from_this() {
        return shared_ptr<T>(weak_this_);
    }

    shared_ptr<const T>
    shared_from_this() const {
        return shared_ptr<const T>(weak_this_);
    }

    weak_ptr<T> weak_from_this() {
        return weak_this_;
    }

    weak_ptr<T const> weak_from_this() const {
        return weak_this_;
    }

private:
    template <typename T1>
    void weak_assign(T1* p, const shared_count& n) const noexcept {
        weak_this_.assign(p, n);
    }

    template <typename T1>
    friend void enable_shared_from_this_helper(const shared_count& pn,
                                               const enable_shared_from_this* pe,
                                               const T1* px) noexcept {
        if (pe)
            pe->weak_assign(const_cast<T1*>(px), pn);
    }

    mutable weak_ptr<T> weak_this_;
};

/**
 *  @brief  Create an object that is owned by a shared_ptr.
 *  @param  a     An allocator.
 *  @param  args  Arguments for the @a T object's constructor.
 *  @return A shared_ptr that owns the newly created object.
 *  @throw  An exception thrown from @a Alloc::allocate or from the
 *          constructor of @a T.
 *
 *  A copy of @a a will be used to allocate memory for the shared_ptr
 *  and the new object.
 */
template <typename T, typename Alloc, typename... Args>
inline shared_ptr<T> allocate_shared(const Alloc& a, Args&&... args) {
    return shared_ptr<T>(_sp_make_shared_tag(), a, std::forward<Args>(args)...);
};

/**
 *  @brief  Create an object that is owned by a shared_ptr.
 *  @param  args  Arguments for the @a T object's constructor.
 *  @return A shared_ptr that owns the newly created object.
 *  @throw  std::bad_alloc, or an exception thrown from the
 *          constructor of @a T.
 */
template <typename T, typename... Args>
inline shared_ptr<T> make_shared(Args&&... args) {
    return rwx::allocate_shared<T>(std::allocator<typename std::remove_const<T>::type>(),
                                   std::forward<Args>(args)...);
};

// shared_ptr I/O
template <typename CharT, typename Traits, typename T>
inline std::basic_ostream<CharT, Traits>&
operator<<(std::basic_ostream<CharT, Traits>& os, const shared_ptr<T>& p) {
    os << p.get();
    return os;
};

// shared_ptr get_deleter
template <typename Deleter, typename T>
inline Deleter* get_deleter(const shared_ptr<T>& p) noexcept {
    return static_cast<Deleter*>(p.get_deleter(typeid(Deleter)));
};

}

namespace std {

template <typename T>
struct less<rwx::shared_ptr<T>>
    : public std::binary_function<rwx::shared_ptr<T>, rwx::shared_ptr<T>, bool>
{
    bool operator()(const rwx::shared_ptr<T>& lhs, const rwx::shared_ptr<T>& rhs) const noexcept {
        return std::less<typename rwx::shared_ptr<T>::element_type*>()(lhs.get(), rhs.get());
    }
};

// std::swap specialization for shared_ptr.
template <typename T>
inline void swap(rwx::shared_ptr<T>& a, rwx::shared_ptr<T>& b) noexcept {
    a.swap(b);
}

// std::hash specialization for shared_ptr.
template <typename T>
struct hash<rwx::shared_ptr<T>> {
    using result_type = std::size_t;
    using argument_type = rwx::shared_ptr<T>;

    std::size_t operator()(const rwx::shared_ptr<T>& s) const noexcept {
        return std::hash<T*>()(s.get());
    }
};

// std::swap specialization for weak_ptr.
template <typename T>
inline void swap(rwx::weak_ptr<T>& a, rwx::weak_ptr<T>& b) noexcept {
    a.swap(b);
}

}
