#pragma strong_types
#define BAD_CALL(value) value
void run()
{
    int count = BAD_CALL(
        "three"
    );
}
