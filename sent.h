struct sentence {
    char *verb;
    struct object *ob;
    char *function;
    struct sentence *next;
};
