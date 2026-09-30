#pragma strong_types
#define BAD_VALUE "three"
void run()
{
    int count = BAD_VALUE
#undef BAD_VALUE
    ;
}
