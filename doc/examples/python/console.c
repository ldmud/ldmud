#include <input_to.h>

private void python_input(string str, int inloop)
{
    string* result = python_console(str || "");

    write(result[0]);

    if(inloop && result[1])
        input_to(#'python_input, INPUT_PROMPT, result[1], 1);
}

private int python(string str)
{
    python_input(str, !sizeof(str));
    return 1;
}

void init()
{
    add_action(#'python, "python");
}
