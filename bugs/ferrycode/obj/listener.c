status mtListening;
status mtLinkSet;
object moLink;

void reset(int iArg) {
  if(iArg) return;
  enable_commands();
}

void catch_tell(string sMsg) {
  if(!mtListening)
    return;

  if(!moLink)
    return;

  moLink->catch_tell(sMsg);
}

void init() {
  if(mtLinkSet && !moLink) {
    destruct(this_object());
  }
}

void set_link(object oLink) {
  moLink = oLink;
  mtLinkSet = 1;
}

void set_listen(status tArg) {
  mtListening = tArg;
}

int query_level() {
  return 100;
}

