#include "catch.hpp"
#include "shared_ptr.h"

struct A {
    A() { ++ctor_count; }
    virtual ~A() { ++dtor_count; }
    static long ctor_count;
    static long dtor_count;
};
long A::ctor_count = 0;
long A::dtor_count = 0;

struct B : A {
    B() { ++ctor_count; }
    virtual ~B() { ++dtor_count; }
    static long ctor_count;
    static long dtor_count;
};
long B::ctor_count = 0;
long B::dtor_count = 0;

struct reset_count_struct {
    ~reset_count_struct()
    {
        A::ctor_count = 0;
        A::dtor_count = 0;
        B::ctor_count = 0;
        B::dtor_count = 0;
    }
};

struct C {};
struct D {};

struct E {
    void operator()(B* p) { delete p; ++delete_count; }
    static long delete_count;
};
long E::delete_count = 0;

TEST_CASE("assign", "[shared_ptr]") {
    SECTION("Assignment from shared_ptr<Y>") {
        reset_count_struct reset;

        rwx::shared_ptr<A> a;

        a = rwx::shared_ptr<A>(new A);
        REQUIRE(a.get() != 0);
        REQUIRE(A::ctor_count == 1);
        REQUIRE(A::dtor_count == 0);

        a = rwx::shared_ptr<A>();
        REQUIRE(a.get() == 0);
        REQUIRE(A::ctor_count == 1);
        REQUIRE(A::dtor_count == 1);
    }

    SECTION("shared_ptr template assignment and void") {
        rwx::shared_ptr<void> p;
        p.operator=<void>(p);
    }

    SECTION("Rvalue assignment from shared_ptr") {
        reset_count_struct reset;

        rwx::shared_ptr<A> a1;
        rwx::shared_ptr<A> a2(new A);

        a1 = std::move(a2);
        REQUIRE( a1.get() != 0 );
        REQUIRE( a2.get() == 0 );
        REQUIRE( a1.use_count() == 1 );
        REQUIRE( a2.use_count() == 0 );
        REQUIRE( A::ctor_count == 1 );
        REQUIRE( A::dtor_count == 0 );

        a1 = std::move(rwx::shared_ptr<A>());
        REQUIRE( a1.get() == 0 );
        REQUIRE( A::ctor_count == 1 );
        REQUIRE( A::dtor_count == 1 );
    }

    SECTION("Rvalue assignment from shared_ptr<Y>") {
        reset_count_struct reset;

        rwx::shared_ptr<A> a;
        rwx::shared_ptr<B> b(new B);

        a = std::move(b);
        REQUIRE( a.get() != 0 );
        REQUIRE( b.get() == 0 );
        REQUIRE( a.use_count() == 1 );
        REQUIRE( b.use_count() == 0 );
        REQUIRE( A::ctor_count == 1 );
        REQUIRE( A::dtor_count == 0 );
        REQUIRE( B::ctor_count == 1 );
        REQUIRE( B::dtor_count == 0 );

        a = std::move(rwx::shared_ptr<A>());
        REQUIRE( a.get() == 0 );
        REQUIRE( A::ctor_count == 1 );
        REQUIRE( A::dtor_count == 1 );
        REQUIRE( B::ctor_count == 1 );
        REQUIRE( B::dtor_count == 1 );
    }

    SECTION("Assignment from shared_ptr<Y>") {
        reset_count_struct reset;

        rwx::shared_ptr<A> a;

        a = rwx::shared_ptr<A>();
        REQUIRE( a.get() == 0 );
        REQUIRE( A::ctor_count == 0 );
        REQUIRE( A::dtor_count == 0 );
        REQUIRE( B::ctor_count == 0 );
        REQUIRE( B::dtor_count == 0 );

        a = rwx::shared_ptr<A>(new A);
        REQUIRE( a.get() != 0 );
        REQUIRE( A::ctor_count == 1 );
        REQUIRE( A::dtor_count == 0 );
        REQUIRE( B::ctor_count == 0 );
        REQUIRE( B::dtor_count == 0 );

        a = rwx::shared_ptr<B>(new B);
        REQUIRE( a.get() != 0 );
        REQUIRE( A::ctor_count == 2 );
        REQUIRE( A::dtor_count == 1 );
        REQUIRE( B::ctor_count == 1 );
        REQUIRE( B::dtor_count == 0 );
    }

    SECTION("Assignment from incompatible shared_ptr<Y>") {
        rwx::shared_ptr<C> a;
        rwx::shared_ptr<D> b;
        // compile error
        // a = b;
    }


    SECTION("std::hash") {
        struct T {};

        rwx::shared_ptr<T> s(new T);
        std::hash<rwx::shared_ptr<T>> hs;
        std::hash<T*> hp;

        REQUIRE( hs(s) == hp(s.get()) );
    }

    SECTION("operator<<") {
        rwx::shared_ptr<A> p(new A);
        std::ostringstream buf;
        buf << p;
        const std::string s = buf.str();
        buf.str("");
        buf << p.get();
        REQUIRE( s == buf.str() );
    }

    SECTION("std::swap") {
        A * const a1 = new A;
        A * const a2 = new A;
        rwx::shared_ptr<A> p1(a1);
        rwx::shared_ptr<A> p2(a2);
        std::swap(p1, p2);
        REQUIRE( p1.get() == a2 );
        REQUIRE( p2.get() == a1 );
    }

    SECTION("conversion to bool") {
        {
            const rwx::shared_ptr<A> p1;
            REQUIRE(static_cast<bool>(p1) == false);
            const rwx::shared_ptr<A> p2(p1);
            REQUIRE(static_cast<bool>(p2) == false);
        }

        {
            rwx::shared_ptr<A> p1(new A);
            REQUIRE( static_cast<bool>(p1) );
            rwx::shared_ptr<A> p2(p1);
            REQUIRE( static_cast<bool>(p2) );
            p1.reset();
            REQUIRE( !static_cast<bool>(p1) );
            REQUIRE( static_cast<bool>(p2) );
        }

        {
            rwx::shared_ptr<A> p1(new A);
            rwx::shared_ptr<A> p2(p1);
            p2.reset(new A);
            REQUIRE( static_cast<bool>(p1) );
            REQUIRE( static_cast<bool>(p2) );
        }
    }

    SECTION("get") {
        struct A {
            A() : i() {}
            int i;
        };

        {
            A *const a = new A;
            const rwx::shared_ptr<A> p(a);
            REQUIRE(p.get() == a);
        }

        {
            A * const a = new A;
            const rwx::shared_ptr<A> p(a);
            REQUIRE( &*p == a );
        }

        {
            A * const a = new A;
            const rwx::shared_ptr<A> p(a);
            REQUIRE( &p->i == &a->i );
        }

        {
            rwx::shared_ptr<int> p;
            auto np = p.operator->();
            REQUIRE( np == nullptr );
        }
    }

    SECTION("owner_before") {
        struct A {
            int i;
            virtual ~A() {}
        };

        struct B : A {};

        SECTION("test empty shared_ptrs compare equivalent") {
            rwx::shared_ptr<A> p1;
            rwx::shared_ptr<B> p2;
            REQUIRE( (!p1.owner_before(p2) && !p2.owner_before(p1)) );
        }

        SECTION("Construction from pointer") {
            rwx::shared_ptr<A> a0;

            rwx::shared_ptr<A> a1(new A);
            REQUIRE( (a1.owner_before(a0) || a0.owner_before(a1)) );
            REQUIRE( !(a1.owner_before(a0) && a0.owner_before(a1)) );

            rwx::shared_ptr<B> b1(new B);
            REQUIRE( (a1.owner_before(b1) || b1.owner_before(a1)) );
            REQUIRE( !(a1.owner_before(b1) && b1.owner_before(a1)) );

            rwx::shared_ptr<A> a2(a1);
            REQUIRE( (!a1.owner_before(a2) && !a2.owner_before(a1)) );
            a2 = b1;
            REQUIRE( (!b1.owner_before(a2) && !a2.owner_before(b1)) );

            rwx::weak_ptr<A> w1(a1);
            REQUIRE( (!a1.owner_before(w1) && !w1.owner_before(a1)) );
            rwx::weak_ptr<A> w2(a2);
            REQUIRE( (!b1.owner_before(w2) && !w2.owner_before(b1)) );
        }

        SECTION("Aliasing") {
            rwx::shared_ptr<A> p1(new A());
            rwx::shared_ptr<int> p2(p1, &p1->i);
            REQUIRE( (!p1.owner_before(p2) && !p2.owner_before(p1)) );
        }
    }

    SECTION("use_count") {
        {
            const rwx::shared_ptr<A> p1;
            REQUIRE( p1.use_count() == 0 );
            const rwx::shared_ptr<A> p2(p1);
            REQUIRE( p1.use_count() == 0 );
        }

        {
            rwx::shared_ptr<A> p1(new A);
            rwx::shared_ptr<A> p2(p1);
            p1.reset();
            REQUIRE( p1.use_count() == 0 );
            REQUIRE( p2.use_count() == 1 );
        }

        {
            rwx::shared_ptr<A> p1(new A);
            rwx::shared_ptr<A> p2(p1);
            p2.reset(new B);
            REQUIRE( p1.use_count() == 1 );
            REQUIRE( p2.use_count() == 1 );
        }
    }

    SECTION("reset") {
        {
            A *const a = new A;
            rwx::shared_ptr<A> p1(a);
            rwx::shared_ptr<A> p2(p1);
            p1.reset();
            REQUIRE(p1.get() == 0);
            REQUIRE(p2.get() == a);
        }

        {
            A * const a = new A;
            B * const b = new B;
            rwx::shared_ptr<A> p1(a);
            rwx::shared_ptr<A> p2(p1);
            p1.reset(b);
            REQUIRE( p1.get() == b );
            REQUIRE( p2.get() == a );
        }

        {
            {
                rwx::shared_ptr<A> p1;
                p1.reset(new B, E());
            }
            REQUIRE( E::delete_count == 1 );
        }
    }

    SECTION("get_deleter") {
        rwx::shared_ptr<int> sp;
        REQUIRE( !rwx::get_deleter<void(*)(int*)>(sp) );
    }

    SECTION("make_shared") {
        struct A {};

        struct B {
            explicit B(int i) : i(i) { }
            int i;
        };

        rwx::shared_ptr<A> spA = rwx::make_shared<A>();
        REQUIRE(spA.get() != 0);

        rwx::shared_ptr<B> spB = rwx::make_shared<B>(99);
        REQUIRE( spB->i == 99 );
    }

}
