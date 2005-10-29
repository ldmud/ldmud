struct wiz_list {
    char *name;
    int length;
    struct wiz_list *next;
    int score;
    int cost;
    int heart_beats;
    int total_worth;
};

extern struct wiz_list *add_name();
extern void save_wiz_file(), load_wiz_file(), wizlist(), wiz_decay();
