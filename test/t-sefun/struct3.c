// Normal use of a simul-efun struct.
// We will load and destroy this multiple times to verify refcounting.

struct SomeStruct value = (<SomeStruct> "Struct");

string get_value() { return value.text; }

void run_test() { get_value(); }
