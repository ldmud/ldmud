/* Check that we can use the structs from the new simul-efun. */
struct NewStruct newval = (<NewStruct> 100);
struct SomeStruct someval = (<SomeStruct> "Test");

int run_test()
{
    return newval.new_member == 100 && someval.text == "Test";
}
