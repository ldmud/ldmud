#pragma save_types, rtt_checks

inherit "compound_defs";

struct base create_struct1()
{
    struct compound c = (<compound> ({ (<base>) }) );
    return c.member[0];
}

struct base create_struct2()
{
    struct sub c = (<sub>);
    return c;
}
