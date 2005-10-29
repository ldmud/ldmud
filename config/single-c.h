/*
 * The interface is not needed for single user interface !
 */

struct interactive {
    int dummy;
    struct sentence *input_to;	/* To be called with next input line ! */
    char *prompt;
};

