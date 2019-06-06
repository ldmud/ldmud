void read_stdin();
void parsefont(register struct fstate *f,char *s0);
int count_spaces(int current, char *buffer);
int compute_code_target();
int compute_label_target();
void dump_line();
void fill_buffer();
void diag(int level,const char *msg, ...); 
int pad_output(int current,int target);
void read_file(char *filename);
void writefdef(register struct fstate *f,int nm);



