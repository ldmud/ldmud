/* Tests the rtt checks when creating structs.
 */

#pragma strong_types, save_types, rtt_checks

struct A
{
    int number;
    object ob;
    mapping|int* data;
};

int run_test()
{
    mixed a_number = 100;
    mixed an_object = this_object();
    mixed a_mapping = ([ "A", "B", "C" ]);
    mixed an_int_array = ({ 11, 12, 13 });
    mixed an_array = ({ "A", "B", "C" });

    if (catch((<A>  a_number,  an_object, a_mapping); publish)
     || catch((<A>  a_number,  an_object, an_int_array); publish))
        return 0;

    if (!catch((<A> an_object, an_object, a_mapping); publish)
     || !catch((<A> a_number,  a_number,  a_mapping); publish)
     || !catch((<A> a_number,  an_object, an_array); publish))
        return 0;

    if (catch((<A> number: a_number, ob: an_object, data: a_mapping); publish)
     || catch((<A> data: an_int_array); publish))
        return 0;

    if (!catch((<A> number: an_object); publish)
     || !catch((<A> number: an_object, ob: an_object); publish)
     || !catch((<A> ob: a_number); publish)
     || !catch((<A> number: a_number, ob: a_number); publish)
     || !catch((<A> data: an_array); publish)
     || !catch((<A> number: a_number, data: an_array); publish))
        return 0;

    if (catch(to_struct(({ a_number,  an_object, a_mapping   }),(<A>)); publish)
     || catch(to_struct(({ a_number,  an_object, an_int_array}),(<A>)); publish))
        return 0;

    if (!catch(to_struct(({an_object, an_object, a_mapping   }),(<A>)); publish)
     || !catch(to_struct(({a_number,  a_number,  a_mapping   }),(<A>)); publish)
     || !catch(to_struct(({a_number,  an_object, an_array    }),(<A>)); publish))
        return 0;

    if (catch(to_struct((["number": a_number, "ob": an_object, "data": a_mapping]),(<A>)); publish)
     || catch(to_struct((["data": an_int_array]),(<A>)); publish)
     || catch(to_struct((["number"]),(<A>)); publish)
     || catch(to_struct((["data": 1;2;3]),(<A>)); publish))
        return 0;

    if (!catch(to_struct((["number": an_object]),(<A>)); publish)
     || !catch(to_struct((["number": an_object, "ob": an_object]),(<A>)); publish)
     || !catch(to_struct((["ob": a_number]),(<A>)); publish)
     || !catch(to_struct((["number": a_number, "ob": a_number]),(<A>)); publish)
     || !catch(to_struct((["ob"]),(<A>)); publish)
     || !catch(to_struct((["data": an_array]),(<A>)); publish)
     || !catch(to_struct((["number": a_number, "data": an_array]),(<A>)); publish)
     || !catch(to_struct((["data": "A";"B";"C"]),(<A>)); publish))
        return 0;

    return 1;
}
