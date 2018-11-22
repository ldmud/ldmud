// First some functions that use the bytes type.
bytes get_bytes()
{
    return to_bytes("ABC", "ascii");
}

#define bytes string    // We try to irritate the compiler.
#pragma no_bytes_type
string duplicate(string val)
{
    return val + val;
}

#pragma bytes_type
#undef bytes
int run_test()
{
    bytes val1 = get_bytes();
    bytes val2 = duplicate(val1);
    return sizeof(val2) == 6;
}
