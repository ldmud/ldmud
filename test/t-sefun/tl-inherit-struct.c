/* Check that we can inherit a struct from the simul-efun. */
struct MyStruct (SomeStruct)
{
    int number;
};

struct MyStruct val1 = (<MyStruct> text: "Test1", number: 11);
struct MyStruct val2 = (<MyStruct> "Test2", 12);

int run_test()
{
    return val1.text == "Test1" && val1.number = 11
        && val2.text == "Test2" && val2.number = 12;
}
