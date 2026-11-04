#pragma strong_types
int take(int value) { return value; }
string inner() { return "three"; }
void run()
{
    take(inner());
}
