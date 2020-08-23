#pragma strict_types

struct Base {};
struct Sub(Base) {};

void sfun(struct Sub s) {}

void fun()
{
    // The compiler should disallow that.
    sfun((<Base>));
}
