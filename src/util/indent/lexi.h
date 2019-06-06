enum rwcodes {
  rw_break,
  rw_switch,
  rw_case,
  rw_struct_like, /* struct, enum, union */
  rw_decl,
  rw_sp_paren, /* if, while, for */
  rw_sp_nparen, /* do, else */
  rw_sizeof
  };

void addkey(char *key,enum rwcodes val);
enum codes lexi();

