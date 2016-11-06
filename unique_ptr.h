#pragma once

#include <type_traits>
#include <tuple>

namespace rwx {

template <typename T>
struct default_delete {
    constexpr default_delete() noexcept = default;

    template <typename U, typename = std::enable_if<std::is_convertible<U*, T*>::value>::type>
    default_delete(const default_delete<U>&) noexcept {};

    void operator()(T* ptr) const {
        static_assert(not std::is_void<T>::value, "can't delete pointer to incomplete type");
        static_assert(sizeof(T) > 0, "can't delete pointer to incomplete type");
        delete ptr;
    }
};

template <typename T>
struct default_delete<T[]> {
    constexpr default_delete() noexcept = default;

    template <typename U, typename = std::enable_if<std::is_convertible<U(*)[], T(*)[]>::value>::type>
    default_delete(const default_delete<U[]>&) noexcept {};

    template <typename U>
    typename std::enable_if<std::is_convertible<U(*)[], T(*)[]>::value>::type
    operator()(U* ptr) const {
        static_assert(sizeof(T) > 0, "can't delete pointer to incomplete type");
        delete [] ptr;
    };
};

// use SFINAE to determine whether Del::pointer exists
template <typename T, typename Deleter>
class Pointer {
    template <typename U>
    static typename U::pointer test(typename U::pointer*);

    template <typename U>
    static T* test(...);

    using Del = typename std::remove_reference<Deleter>::type;

public:
    using type = decltype(test<Del>(0));
};

#if __cplusplus > 201402L

using std::conjuction;
using std::disjunction;
using std::negation;

#else

template <typename...> struct conjunction : std::true_type {};
template <typename B1> struct conjunction<B1> : B1 {};
template <typename B1, typename... Bn>
struct conjunction<B1, Bn...> : std::conditional_t<B1::value != false, conjunction<Bn...>, B1> {};

template <typename...> struct disjunction : std::false_type {};
template <typename B1> struct disjunction<B1> : B1 {};
template <typename B1, typename... Bn>
struct disjunction<B1, Bn...> : std::conditional_t<B1::value != false, B1, disjunction<Bn...>>  {};

template <class B>
struct negation : std::bool_constant<!B::value> {};

#endif

template<typename... Cond>
using require = typename std::enable_if<conjunction<Cond...>::value>::type;

// unique_ptr for single objects.
template <typename T, typename Deleter = default_delete<T>>
class unique_ptr {
    using tuple_type = std::tuple<typename Pointer<T, Deleter>::type, Deleter>;

    tuple_type t_;

    // helper template for detecting a safe conversion from another
    // unique_ptr
    template <typename U, typename E>
    using safe_conversion_up = conjunction<
        std::is_convertible<typename unique_ptr<U, E>::pointer, pointer>,
        negation<std::is_array<U>>,
        disjunction<
            conjunction<std::is_reference<deleter_type>, std::is_same<E, deleter_type>>,
            conjunction<negation<std::is_reference<deleter_type>>, std::is_convertible<E, deleter_type>>
        >
    >;

public:
    using pointer = typename Pointer::type;
    using element_type = T;
    using deleter_type = Deleter;

    // Constructors.

    /// Default constructor, creates a unique_ptr that owns nothing.
    constexpr unique_ptr() noexcept : t_() {
        static_assert(!std::is_pointer<deleter_type>::value, "constructed with null function pointer deleter");
    }

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an object of @c element_type
     *
     * The deleter will be value-initialized.
     */
    explicit unique_ptr(pointer p) noexcept : t_(p, deleter_type()) {
        static_assert(!is_pointer<deleter_type>::value, "constructed with null function pointer deleter");
    }

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an object of @c element_type
     * @param d  A reference to a deleter.
     *
     * The deleter will be initialized with @p d
     */
    unique_ptr(pointer p,
               typename std::conditional<std::is_reference<deleter_type>::value,
                                          deleter_type,
                                          const deleter_type&
                                         >::type d) noexcept
        : t_(p, d) {}

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an object of @c element_type
     * @param d  An rvalue reference to a deleter.
     *
     * The deleter will be initialized with @p std::move(d)
     */
    unique_ptr(pointer p,
               typename std::remove_reference<deleter_type>::type&& d) noexcept
        : t_(std::move(p), std::move(d)) {
        static_assert(!std::is_reference<deleter_type>::value, "rvalue deleter bound to reference");
    }

    /// Creates a unique_ptr that owns nothing.
    constexpr unique_ptr(nullptr_t) noexcept : unique_ptr() {}

    // Move constructors.

    /// Move constructor.
    unique_ptr(unique_ptr&& u) noexcept
        : t_(u.release(), std::forward<deleter_type>(u.get_deleter()))
    {}

    /** @brief Converting constructor from another type
     *
     * Requires that the pointer owned by @p u is convertible to the
     * type of pointer owned by this object, @p u does not own an array,
     * and @p u has a compatible deleter type.
     */
    template<typename U, typename E, typename = require<
        safe_conversion_up<U, E>,
        typename std::conditional<std::is_reference<Deleter>::value,
                                   std::is_same<E, Deleter>,
                                   std::is_convertible<E, Deleter>
                                  >::type
        >
    >
    unique_ptr(unique_ptr<U, E>&& u) noexcept
        : t_(u.release(), std::forward<E>(u.get_deleter()))
    {}

    /// Destructor, invokes the deleter if the stored pointer is not null.
    ~unique_ptr() noexcept {
        auto& ptr = std::get<0>(t_);
        if (ptr != nullptr)
            get_deleter()(ptr);
        ptr = pointer();
    }

    // Assignment.

    /** @brief Move assignment operator.
     *
     * @param u  The object to transfer ownership from.
     *
     * Invokes the deleter first if this object owns a pointer.
     */
    unique_ptr& operator=(unique_ptr&& u) noexcept {
        reset(u.release());
        get_deleter() = std::forward<deleter_type>(u.get_deleter());
        return *this;
    }

    /** @brief Assignment from another type.
     *
     * @param u  The object to transfer ownership from, which owns a
     *             convertible pointer to a non-array object.
     *
     * Invokes the deleter first if this object owns a pointer.
     */
    template<typename U, typename E>
    typename std::enable_if<conjunction<safe_conversion_up<U, E>,
                                         std::is_assignable<deleter_type&, E&&>
                                        >::value,
                             unique_ptr&
    >::type
    operator=(unique_ptr<U, E>&& u) noexcept
    {
        reset(u.release());
        get_deleter() = std::forward<E>(u.get_deleter());
        return *this;
    }

    /// Reset the %unique_ptr to empty, invoking the deleter if necessary.
    unique_ptr& operator=(nullptr_t) noexcept
    {
        reset();
        return *this;
    }

    // Observers.

    /// Dereference the stored pointer.
    typename std::add_lvalue_reference<element_type>::type
    operator*() const {
        return *get();
    }

    /// Return the stored pointer.
    pointer operator->() const noexcept {
        return get();
    }

    /// Return the stored pointer.
    pointer get() const noexcept {
        return std::get<0>(t_);
    }

    /// Return a reference to the stored deleter.
    deleter_type& get_deleter() noexcept {
        return std::get<1>(t_);
    }

    /// Return a reference to the stored deleter.
    const deleter_type& get_deleter() const noexcept {
        return std::get<1>(t_);
    }

    /// Return @c true if the stored pointer is not null.
    explicit operator bool() const noexcept {
        return get() != pointer();
    }

    // Modifiers.

    /// Release ownership of any stored pointer.
    pointer release() noexcept {
        pointer p = get();
        std::get<0>(t_) = pointer();
        return p;
    }

    /** @brief Replace the stored pointer.
     *
     * @param p  The new pointer to store.
     *
     * The deleter will be invoked if a pointer is already owned.
     */
    void reset(pointer p = pointer()) noexcept {
        using std::swap;
        swap(std::get<0>(t_), p);
        if (p != pointer())
            get_deleter()(p);
    }

    /// Exchange the pointer and deleter with another object.
    void swap(unique_ptr& u) noexcept {
        using std::swap;
        swap(t_, u.t_);
    }

    // Disable copy from lvalue.
    unique_ptr(const unique_ptr&) = delete;
    unique_ptr& operator=(const unique_ptr&) = delete;
};

/// unique_ptr for array objects with a runtime length
// omit specialization for array objects with a compile time length
template<typename T, typename Deleter>
class unique_ptr<T[], Deleter> {
    using tuple_type = std::tuple<typename Pointer<T, Deleter>::type, Deleter>;

    tuple_type t_;

    template <typename U>
    using remove_cv = typename std::remove_cv<U>::type;

    // like is_base_of<T, U> but false if unqualified types are the same
    template<typename U>
    using is_derivedT = conjunction<std::is_base_of<T, U>,
                                    negation<std::is_same<remove_cv<T>, remove_cv<U>>>>;

public:
    using pointer = typename Pointer::type;
    using element_type = T;
    using deleter_type = Deleter;

    // helper template for detecting a safe conversion from another
    // unique_ptr
    template <typename U, typename E,
        typename U_unique_ptr = unique_ptr<U, E>,
        typename U_element_type = typename U_unique_ptr::element_type
    >
    using safe_conversion_up = conjunction<
        std::is_array<U>,
        std::is_same<pointer, element_type*>,
        std::is_same<typename U_unique_ptr::pointer, U_element_type*>,
        std::is_convertible<U_element_type(*)[], element_type(*)[]>,
        disjunction<
            conjunction<std::is_reference<deleter_type>, std::is_same<E, deleter_type>>,
            conjunction<negation<std::is_reference<deleter_type>>, std::is_convertible<E, deleter_type>>
        >
    >;

    // helper template for detecting a safe conversion from a raw pointer
    template <typename U>
    using safe_conversion_raw = conjunction<
        disjunction<
            disjunction<std::is_same<U, pointer>, std::is_same<U, nullptr_t>>,
            conjunction<
                std::is_pointer<U>,
                std::is_same<pointer, element_type*>,
                std::is_convertible<typename std::remove_pointer<U>::type(*)[], element_type(*)[]>
            >
        >
    >;

    // Constructors.

    /// Default constructor, creates a unique_ptr that owns nothing.
    constexpr unique_ptr() noexcept : t_() {
        static_assert(!std::is_pointer<deleter_type>::value, "constructed with null function pointer deleter");
    }

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an array of a type safely convertible
     * to an array of @c element_type
     *
     * The deleter will be value-initialized.
     */
    template<typename U, typename = typename std::enable_if<safe_conversion_raw<U>::value, bool>::type>
    explicit unique_ptr(U p) noexcept : t_(p, deleter_type()) {
        static_assert(!is_pointer<deleter_type>::value, "constructed with null function pointer deleter");
    }

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an array of a type safely convertible
     * to an array of @c element_type
     * @param d  A reference to a deleter.
     *
     * The deleter will be initialized with @p d
     */
    template<typename U, typename = typename std::enable_if<safe_conversion_raw<U>::value, bool>::type>
    unique_ptr(U p,
               typename std::conditional<std::is_reference<deleter_type>::value,
                   deleter_type,
                   const deleter_type&
               >::type d) noexcept
        : t_(p, d)
    {}

    /** Takes ownership of a pointer.
     *
     * @param p  A pointer to an array of a type safely convertible
     * to an array of @c element_type
     * @param d  A reference to a deleter.
     *
     * The deleter will be initialized with @p std::move(d)
     */
    template<typename U, typename = typename std::enable_if<safe_conversion_raw<U>::value, bool>::type>
    unique_ptr(U p, typename std::remove_reference<deleter_type>::type&& d) noexcept
        : t_(std::move(p), std::move(d)) {
        static_assert(!std::is_reference<deleter_type>::value, "rvalue deleter bound to reference");
    }

    /// Move constructor.
    unique_ptr(unique_ptr&& u) noexcept
        : t_(u.release(), std::forward<deleter_type>(u.get_deleter()))
    {}

    /// Creates a unique_ptr that owns nothing.
    constexpr unique_ptr(nullptr_t) noexcept : unique_ptr() {}

    template<typename U, typename E,
        typename = require<safe_conversion_up<U, E>>>
    unique_ptr(unique_ptr<U, E>&& u) noexcept
        : t_(u.release(), std::forward<E>(u.get_deleter()))
    {}

    /// Destructor, invokes the deleter if the stored pointer is not null.
    ~unique_ptr() {
        auto& ptr = std::get<0>(t_);
        if (ptr != nullptr)
            get_deleter()(ptr);
        ptr = pointer();
    }

    // Assignment.

    /** @brief Move assignment operator.
     *
     * @param u  The object to transfer ownership from.
     *
     * Invokes the deleter first if this object owns a pointer.
     */
    unique_ptr& operator=(unique_ptr&& u) noexcept {
        reset(u.release());
        get_deleter() = std::forward<deleter_type>(u.get_deleter());
        return *this;
    }

    /** @brief Assignment from another type.
     *
     * @param u  The object to transfer ownership from, which owns a
     *             convertible pointer to an array object.
     *
     * Invokes the deleter first if this object owns a pointer.
     */
    template<typename U, typename E>
    typename std::enable_if<
        conjunction<
            safe_conversion_up<U, E>,
            std::is_assignable<deleter_type&, E&&>
        >::value,
        unique_ptr&
    >::type
    operator=(unique_ptr<U, E>&& u) noexcept {
        reset(u.release());
        get_deleter() = std::forward<E>(u.get_deleter());
        return *this;
    }

    /// Reset the %unique_ptr to empty, invoking the deleter if necessary.
    unique_ptr& operator=(nullptr_t) noexcept {
        reset();
        return *this;
    }

    // Observers.

    /// Access an element of owned array.
    typename std::add_lvalue_reference<element_type>::type
    operator[](size_t i) const {
        return get()[i];
    }

    /// Return the stored pointer.
    pointer get() const noexcept {
        return std::get<0>(t_);
    }

    /// Return a reference to the stored deleter.
    deleter_type& get_deleter() noexcept {
        return std::get<1>(t_);
    }

    /// Return a reference to the stored deleter.
    const deleter_type& get_deleter() const noexcept {
        return std::get<1>(t_);
    }

    /// Return @c true if the stored pointer is not null.
    explicit operator bool() const noexcept {
        return get() != pointer();
    }

    // Modifiers.

    /// Release ownership of any stored pointer.
    pointer release() noexcept {
        pointer p = get();
        std::get<0>(t_) = pointer();
        return p;
    }

    /** @brief Replace the stored pointer.
     *
     * @param p  The new pointer to store.
     *
     * The deleter will be invoked if a pointer is already owned.
     */
    template <typename U, typename = require<
        disjunction<
            std::is_same<U, pointer>,
            conjunction<
                std::is_same<pointer, element_type*>,
                std::is_pointer<U>,
                std::is_convertible<
                    typename std::remove_pointer<U>::type(*)[],
                    element_type(*)[]
                >
            >
        >
    >>
    void reset(U p) noexcept {
        using std::swap;
        swap(std::get<0>(t_), p);
        if (p != nullptr)
            get_deleter()(p);
    }

    void reset(nullptr_t = nullptr) noexcept {
        reset(pointer());
    }

    /// Exchange the pointer and deleter with another object.
    void swap(unique_ptr& u) noexcept {
        using std::swap;
        swap(t_, u.t_);
    }

    // Disable copy from lvalue.
    unique_ptr(const unique_ptr&) = delete;
    unique_ptr& operator=(const unique_ptr&) = delete;
};

template<typename T, typename Deleter, typename U, typename E>
inline bool operator==(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    return x.get() == y.get();
}

template<typename T, typename Deleter>
inline bool operator==(const unique_ptr<T, Deleter>& x, nullptr_t) noexcept {
    return !x;
}

template<typename T, typename Deleter>
inline bool operator==(nullptr_t, const unique_ptr<T, Deleter>& x) noexcept {
    return !x;
}

template<typename T, typename Deleter, typename U, typename E>
inline bool operator!=(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    return x.get() != y.get();
}

template<typename T, typename Deleter>
inline bool operator!=(const unique_ptr<T, Deleter>& x, nullptr_t) noexcept {
    return (bool)x;
}

template<typename T, typename Deleter>
inline bool operator!=(nullptr_t, const unique_ptr<T, Deleter>& x) noexcept {
    return (bool)x;
}

template<typename T, typename Deleter, typename U, typename E>
inline bool operator<(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    using CT = typename std::common_type<typename unique_ptr<T, Deleter>::pointer,
                                          typename unique_ptr<U, E>::pointer
                                         >::type;
    return std::less<CT>()(x.get(), y.get());
}

template<typename T, typename Deleter>
inline bool operator<(const unique_ptr<T, Deleter>& x, nullptr_t) {
    return std::less<typename unique_ptr<T, Deleter>::pointer>()(x.get(), nullptr);
}

template<typename T, typename Deleter>
inline bool operator<(nullptr_t, const unique_ptr<T, Deleter>& x) {
    return std::less<typename unique_ptr<T, Deleter>::pointer>()(nullptr, x.get());
}

template<typename T, typename Deleter, typename U, typename E>
inline bool operator<=(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    return !(y < x);
}

template<typename T, typename Deleter>
inline bool operator<=(const unique_ptr<T, Deleter>& x, nullptr_t) {
    return !(nullptr < x);
}

template<typename T, typename Deleter>
inline bool operator<=(nullptr_t, const unique_ptr<T, Deleter>& x) {
    return !(x < nullptr);
}

template<typename T, typename Deleter, typename U, typename E>
inline bool operator>(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    return (y < x);
}

template<typename T, typename Deleter>
inline bool operator>(const unique_ptr<T, Deleter>& x, nullptr_t) {
    return std::less<typename unique_ptr<T, Deleter>::pointer>()(nullptr, x.get());
}

template<typename T, typename Deleter>
inline bool operator>(nullptr_t, const unique_ptr<T, Deleter>& x) {
    return std::less<typename unique_ptr<T, Deleter>::pointer>()(x.get(), nullptr);
}

template<typename T, typename Deleter, typename U, typename E>
inline bool operator>=(const unique_ptr<T, Deleter>& x, const unique_ptr<U, E>& y) {
    return !(x < y);
}

template<typename T, typename Deleter>
inline bool operator>=(const unique_ptr<T, Deleter>& x, nullptr_t) {
    return !(x < nullptr);
}

template<typename T, typename Deleter>
inline bool operator>=(nullptr_t, const unique_ptr<T, Deleter>& x) {
    return !(nullptr < x);
}

#if __cplusplus > 201103L

template<typename T>
struct _make_unique {
    typedef unique_ptr<T> single_object;
};

template<typename T>
struct _make_unique<T[]> {
    typedef unique_ptr<T[]> array;
};

template<typename T, size_t Bound>
struct _make_unique<T[Bound]> {
    struct invalid_type {};
};

/// std::make_unique for single objects
template<typename T, typename... Args>
inline typename _make_unique<T>::single_object
make_unique(Args&&... args) {
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
}

/// std::make_unique for arrays of unknown bound
template<typename T>
inline typename _make_unique<T>::array
make_unique(size_t num) {
    return unique_ptr<T>(new std::remove_extent_t<T>[num]());
}

/// Disable std::make_unique for arrays of known bound
template<typename T, typename... Args>
inline typename _make_unique<T>::invalid_type
make_unique(Args&&...) = delete;

#endif

}

namespace std {

template<typename T, typename Deleter>
inline void swap(unique_ptr<T, Deleter>& x, unique_ptr<T, Deleter>& y) noexcept {
    x.swap(y);
}

/// std::hash specialization for unique_ptr.
template <typename T, typename Deleter>
struct hash<rwx::unique_ptr<T, Deleter>> {
    using result_type = std::size_t;
    using argument_type = rwx::unique_ptr<T, Deleter>;

    std::size_t operator()(const rwx::unique_ptr<T, Deleter>& u) const noexcept {
        return std::hash<typename rwx::unique_ptr<T, Deleter>::pointer>()(u.get());
    }
};

}
