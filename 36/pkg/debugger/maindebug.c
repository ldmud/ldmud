#define ABM add_message(buf);
#define CLRSCR add_message("\e[2J"); add_message(MESSAGE_FLUSH);

if ((strcmp(buf,"h") == 0) || (strcmp(buf,"hilfe") == 0))
 {
  CLRSCR;
  add_message("Sunblades LPC-Debuggers Ultrakurzhilfeseite\n");
  add_message("-------------------------------------------------------------\n");
  add_message(" h    -> Hilfe\n");
  add_message(" s    -> Status\n");
  add_message(" lv   -> List Vars\n");
  add_message(" lf   -> List Functions\n");
  add_message(" i    -> Inspect Variable\n");
  add_message(" b    -> Breakpoint setzen\n");
  add_message(" n    -> Next Instruction\n");
  add_message(" c    -> Continue to Breakpoint\n");
  add_message(" t    -> Trace into fuction\n");
  add_message(" rtfm -> Read the fine Manual\n");
  add_message(MESSAGE_FLUSH);
 } else
if (strcmp(buf,"rftm") == 0)
 {
  add_message("READ THE FUCKING MANUAL !\n");
 } else
if ((strcmp(buf,"s") == 0) || (strcmp(buf,"status") == 0))
 {
  CLRSCR;
  add_message("Statusinformationen ueber das aktuelle Objekt\n");
  add_message("-------------------------------------------------------------\n");
  sprintf(buf,"                  Name : %s\n",current_object->name); ABM;
  sprintf(buf,"Anz. der globalen Vars : %d\n",current_object->prog->num_variables); ABM;
  sprintf(buf,"   Anz. der Funktionen : %d\n",current_object->prog->num_functions); ABM;
  sprintf(buf,"      Anz. der Strings : %d\n",current_object->prog->num_strings); ABM;
  sprintf(buf,"     Anz. der Inherits : %d\n",current_object->prog->num_inherited); ABM;
  add_message("\n");
  add_message("Statusinformationen ueber die aktuelle Funktion \n");
  add_message("-------------------------------------------------------------\n");
  add_message("                  Name : "); add_message((char *) (*( (char **) (&csp->funstart[-5]) ))); add_message("\n");
  sprintf(buf," Anz. der lokalen Vars : %d\n",(csp->num_local_variables - csp->funstart[0])); ABM;
  sprintf(buf,"    Anz. der Parameter : %d\n",(csp->funstart[0])); ABM;
 } else 
if ((strcmp(buf,"lv") == 0) || (strcmp(buf,"list vars") == 0))
 {
  CLRSCR;
  add_message("Liste der Variablen des aktuellen Objektes\n");
  add_message("-------------------------------------------------------------\n");
   if (current_object)
    {
     add_message("Objektglobale Variablen : \n");
     for (lauf1 = 0;lauf1 < current_object->prog->num_variables;lauf1++)
      {
       sprintf(buf,"%s%n",current_object->prog->variable_names[lauf1].name,&lauf2);
       add_message(buf);
       if (!((lauf1 % 2) != 0))
	{ // *** Da es meist viele Objektglobale Variablen gibt, diese zweispaltig darstellen.
         for (lauf3 = lauf2; lauf3 < (NUMLINES / 2); lauf3++)
	   { add_message(" "); }
	} else { add_message("\n"); }
      }
     if ((lauf1 % 2) != 0) 
      { add_message("\n"); }
     add_message("\n");
//     add_message("Funktionslokale Variablen : \n");
     tempfunktion = locate_function((char *) (*( (char **) (&csp->funstart[-5]) )),current_object->prog);

     if (tempfunktion->variable_names != 0)
      {
       add_message("Funktionslokale Variablen : \n");
       lauf1 = -1;
       do
	{
	 lauf1++;
	 sprintf(buf,"%s%n",tempfunktion->variable_names[lauf1].name,&lauf2);
	 add_message(buf);
         if (!((lauf1 % 2) != 0))
	  { // *** Da es meist viele Objektglobale Variablen gibt, diese zweispaltig darstellen.
           for (lauf3 = lauf2; lauf3 < (NUMLINES / 2); lauf3++)
	     { add_message(" "); }
	  } else { add_message("\n"); }
	} 	 
       while (tempfunktion->variable_names[lauf1].flags != 0xff);
      }
    
    }
 } else
if ((strcmp(buf,"lf") == 0) || (strcmp(buf,"list fun") == 0))
 {
  CLRSCR;
  add_message("Liste der Funktionen des aktuellen Objektes\n");
  add_message("-------------------------------------------------------------\n");
  sprintf(buf,"Anzahl der Funktionen : %d\n",current_object->prog->num_functions); ABM;
  for (lauf1 = 0; lauf1 < current_object->prog->num_functions; lauf1++ )
   {
    printflags(buf2,current_object->prog->funktionsdaten[lauf1].flags);
    sprintf(buf,"%s %s%n",buf2,current_object->prog->funktionsdaten[lauf1].name,&lauf2);
    
    add_message(buf);
    if (!((lauf1 % 2) != 0))
     { // *** Da es meist viele Objektglobale Variablen gibt, diese zweispaltig darstellen.
      for (lauf3 = lauf2; lauf3 < (NUMLINES / 2); lauf3++)
       { add_message(" "); }
     } else { add_message("\n"); }    
   }
 } else
if ((sscanf(buf,"i %s",buf2) == 1) || (sscanf(buf,"inspect %s",buf2) == 1))
 {
  CLRSCR;
  sprintf(buf,"Auswertung der Variablen %s : \n",buf2); ABM;
  add_message("-------------------------------------------------------------\n");
  // *** Erst festestellen. ob die Variable lokal vorhanden ist.
  // *** Sollte dies der Fall sein, dann diese Deklaration der globalen vorziehen.
  tempsvalue = get_local_variable_pointer(buf2);
  if (tempsvalue != 0)
   {
    buf[0] = 0x00;
    get_variable_data_for_svalue(buf,tempsvalue);
    add_message(buf);
   } else
   {
    tempsvalue = get_global_variable_pointer(buf2);
    if (tempsvalue != 0)
     {
      buf[0] = 0x00;
      get_variable_data_for_svalue(buf,tempsvalue);
      add_message(buf);
     } else
     {
      sprintf(buf," Leider konnte die Variable %s nicht gefunden werden !\n",buf2); ABM;
     }
   }
  add_message("\n");
 } else
if ((strcmp(buf,"lb") == 0) || (strcmp(buf,"list break") == 0))
 { // *** Da moechte jemand schaun, welche Breakpoints er schon definiert 
   // *** hat. 
  CLRSCR;
  add_message("Anzeige der aktuell definierten Breakpoints\n");
  add_message("-------------------------------------------------------------\n");
  if (first_breakpoint == 0)
   {
    add_message("Bis jetzt sind noch keine Breakpoints definiert worden.\n");
   } else
   {
    temp_breakpoint = first_breakpoint;
    while (temp_breakpoint != 0) 
     {
      add_message("Nr. %d : Datei %s, Zeile %d\n",temp_breakpoint->number,temp_breakpoint->in_file,temp_breakpoint->line);
      temp_breakpoint = temp_breakpoint->next;	
     }
   }
  add_message("\n");
 } else 
if ((sscanf(buf,"b %s %d",buf2,&lauf1) == 2) || (sscanf(buf,"break %s %d",buf2,&lauf1) == 2))
 { 
  num_breakpoints++;
  sprintf(buf,"Breakpoint Nr. %d wird in der Datei %s, Zeile %d hinzugefuegt.\n",num_breakpoints,buf2,lauf1);
  add_message(buf);
  add_message("\n");
    
  if (first_breakpoint == 0) // *** Bisher keine Breakpoints definiert. 
   {
    temp_breakpoint = malloc(sizeof( struct breakpoint )); 
    first_breakpoint = temp_breakpoint;
   } else
   {
    // *** Den letzten definierten Breakpoint suchen und da anhaengen.
    temp_breakpoint = first_breakpoint;
    while (temp_breakpoint->next != 0)
     { temp_breakpoint = temp_breakpoint->next; }
    temp_breakpoint->next = malloc( sizeof(struct breakpoint ));
    temp_breakpoint = temp_breakpoint->next;
   }
  // *** Die Struktur, in der wir unseren neuen Breakpoint ablegen koennen,
  // *** ist nun in Temp-Breakpoint.
  temp_breakpoint->next = 0; // *** Ich trau dem Mallloc nicht so ganz !
  temp_breakpoint->line = lauf1;
  temp_breakpoint->in_file = malloc(strlen(buf2)+1);
  strcpy(temp_breakpoint->in_file,buf2);
  temp_breakpoint->number = num_breakpoints;  
 } else
if ((strcmp(buf,"c") == 0) || (strcmp(buf,"continue") == 0))
 {
  running_program = 1;
  debug_ok = 1;
  add_message("Programm wird ausgefuehrt, bis ein Breakpoint erreicht wird !\n");
  add_message(MESSAGE_FLUSH);
 } else
if ((strcmp(buf,"n") == 0) || (strcmp(buf,"next") == 0))
 { /* Kein Match gefunden. Weitermachen. */
  debug_ok = 1;
 } else
if ((strcmp(buf,"d") == 0) || (strcmp(buf,"debughook") == 0))
 {
  add_message("Debug !\n");
  add_message(MESSAGE_FLUSH);
 } else
if ((strcmp(buf,"t") == 0) || (strcmp(buf,"trace") == 0))
 {
  debug_ok = 1;
  flags_for_next_line = flags_for_next_line | FLAG_TRACE_INTO_NEXT_FUNCTION;
 } else
if ((strcmp(buf,"fe") == 0) || (strcmp(buf,"funktionsede") == 0))
 {
  debug_ok = 1;
  last_displayed_function->flags = (last_displayed_function->flags & !(FLAG_SHOW_THIS_FUNCTION));
 }
 {
  /* Keine sinnvolle Eingabe -> Banner ausgeben */
  print_banner = 1;
 }

