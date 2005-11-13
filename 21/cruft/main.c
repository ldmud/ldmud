/*-------------------------------------------------------------------------*/
#if 0
void debug_message_svalue(v)
    struct svalue *v;
{
    if (v == 0) {
	debug_message("<NULL>");
	return;
    }
    switch(v->type) {
    case T_NUMBER:
	debug_message("%ld", v->u.number);
	return;
    case T_STRING:
	debug_message("\"%s\"", v->u.string);
	return;
    case T_OBJECT:
	debug_message("OBJ(%s)", v->u.ob->name);
	return;
    case T_LVALUE:
	debug_message("Pointer to ");
	debug_message_svalue(v->u.lvalue);
	return;
    default:
	debug_message("<INVALID>\n");
	return;
    }
}
#endif

