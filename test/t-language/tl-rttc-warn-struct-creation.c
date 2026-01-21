/* Tests the rtt checks when creating structs.
 */

#pragma strong_types, save_types, warn_rtt_checks

struct A
{
    int number;
    object ob;
    mapping|int* data;
};

void fun(struct A a) {} // Just to use the struct.

int run_test()
{
    mixed a_number = 100;
    mixed an_object = this_object();
    mixed a_mapping = ([ "A", "B", "C" ]);
    mixed an_int_array = ({ 11, 12, 13 });
    mixed an_array = ({ "A", "B", "C" });

    fun((<A>  a_number,  an_object, a_mapping));
    fun((<A>  a_number,  an_object, an_int_array));

    if (__MASTER_OBJECT__.rt_warning_occured() != 0)
        return 0;
    
    fun((<A> an_object, an_object, a_mapping));
    fun((<A> a_number,  a_number,  a_mapping));
    fun((<A> a_number,  an_object, an_array));
    fun((<A> an_object, a_number,  an_array));
    if (__MASTER_OBJECT__.rt_warning_occured() != 6)
        return 0;

    fun((<A> number: a_number, ob: an_object, data: a_mapping));
    fun((<A> data: an_int_array));
    if (__MASTER_OBJECT__.rt_warning_occured() != 6)
        return 0;

    fun((<A> number: an_object));
    fun((<A> number: an_object, ob: an_object));
    fun((<A> ob: a_number));
    fun((<A> number: a_number, ob: a_number));
    fun((<A> data: an_array));
    fun((<A> number: a_number, data: an_array));
    fun((<A> number: an_object, ob: a_number, data: an_array));
    if (__MASTER_OBJECT__.rt_warning_occured() != 15)
        return 0;

    fun(to_struct(({ a_number,  an_object, a_mapping   }),(<A>)));
    fun(to_struct(({ a_number,  an_object, an_int_array}),(<A>)));
    if (__MASTER_OBJECT__.rt_warning_occured() != 15)
        return 0;

    fun(to_struct(({an_object, an_object, a_mapping   }),(<A>)));
    fun(to_struct(({a_number,  a_number,  a_mapping   }),(<A>)));
    fun(to_struct(({a_number,  an_object, an_array    }),(<A>)));
    fun(to_struct(({an_object, a_number,  an_array    }),(<A>)));
    if (__MASTER_OBJECT__.rt_warning_occured() != 21)
        return 0;

    fun(to_struct((["number": a_number, "ob": an_object, "data": a_mapping]),(<A>)));
    fun(to_struct((["data": an_int_array]),(<A>)));
    fun(to_struct((["number"]),(<A>)));
    fun(to_struct((["data": 1;2;3]),(<A>)));
    if (__MASTER_OBJECT__.rt_warning_occured() != 21)
        return 0;

    fun(to_struct((["number": an_object]),(<A>)));
    fun(to_struct((["number": an_object, "ob": an_object]),(<A>)));
    fun(to_struct((["ob": a_number]),(<A>)));
    fun(to_struct((["number": a_number, "ob": a_number]),(<A>)));
    fun(to_struct((["ob"]),(<A>)));
    fun(to_struct((["data": an_array]),(<A>)));
    fun(to_struct((["number": a_number, "data": an_array]),(<A>)));
    fun(to_struct((["data": "A";"B";"C"]),(<A>)));
    fun(to_struct((["number": an_object, "ob": a_number, "data": an_array]),(<A>)));
    if (__MASTER_OBJECT__.rt_warning_occured() != 32)
        return 0;

    return 1;
}
