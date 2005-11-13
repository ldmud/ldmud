
struct svalue *f_debugfile( sp )
 struct svalue *sp;
{
 // *** Das ist die efun, die das Debuggen einleitet.
 // *** Als Parameter werden erwartet : 
 // ***  - string prefix
 // ***  - int flags  
 struct object *ob;
 struct interactive *wizard;
 struct object *new_ob;
 int meine_parameter;
   
  /* Parameter ueberpreufen */
  if (sp[-1].type != T_STRING)
    bad_xefun_arg(1,sp);
  if (sp->type != T_NUMBER)
    bad_xefun_arg(2,sp);

   
 /*
  * So, wir haben nun im Parameter 1 (sp[-1]) das Prefix.
  * Im Parameter 2 (sp->) findet sich die Flags !
  * Was ist nun zu tun ?
  * Ganz Einfach - Dei Daten in die interactive-Struktur eintragen.
  */
   
  meine_parameter = sp->u.number; /* Die Parameter retten, weil ich gleich den Stack poppe */
  pop_stack();
  

  wizard = O_GET_INTERACTIVE(command_giver);
   
  // Der Stack steht auf dem Namen
   
  wizard->debug_prefix = make_shared_string(sp->u.string);
  wizard->debug_level = meine_parameter;
                           // *** Im Moment unterstuetze ich noch keine 
                           // *** Flags !
   
  sp->type = T_NUMBER;
  sp->u.number = 1; 
   
  return sp;
}
