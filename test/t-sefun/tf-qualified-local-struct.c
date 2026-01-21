/* Check that a local struct won't be found when using sefun:: */
struct MyStruct
{
};

struct sefun::MyStruct myval;
