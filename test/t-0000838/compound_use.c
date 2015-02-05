#pragma save_types, rtt_checks

inherit "compound_defs";

struct base create_struct()
{
    struct compound c = (<compound> ({ (<base>) }) );
    return c->member[0];
}
