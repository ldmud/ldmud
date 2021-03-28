int run_test()
{
    float x = __FLOAT_MIN__, y = __FLOAT_MAX__;

    return x < 1 && x < y && 1 < y
        && !__MASTER_OBJECT__.warning_occured();
}
