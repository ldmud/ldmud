#pragma strict_types

struct Base {};
struct Sub(Base) {};

void fun()
{
    // The compiler should disallow that.
    struct Sub s = (<Base>);
}
