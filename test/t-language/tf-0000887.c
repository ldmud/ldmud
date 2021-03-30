/* The following code should fail to compile, not crash the driver */

#pragma strong_types

void foo() {
    return "" + b"";
}
