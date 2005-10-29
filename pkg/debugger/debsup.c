#define DEBERROR(x) debug_message(x); return 0;

/*
 * In dieser Datei stehen diverse Routinen die beim Programmieren des
 * base-Debuggers nuetlich sind.
 */

void printflags( char *buffer, uint32 flags )
 { 
  // *** Schreibt die Flags in den Buffer.
  buffer[0] = 0x00;
  strcat(buffer,"[");
    
#define APPEND_WS { strcat(buffer," "); }
  if (flags & NAME_INHERITED) 
      { strcat(buffer,"i"); } else APPEND_WS;
  if (flags & TYPE_MOD_STATIC)
      { strcat(buffer,"S"); } else APPEND_WS;
  if (flags & TYPE_MOD_NO_MASK)
      { strcat(buffer,"N"); } else APPEND_WS;
    
  if (flags & TYPE_MOD_PRIVATE) // *** Pirivate und public schliessen sich aus
      { strcat(buffer,"P"); } else
  if (flags & TYPE_MOD_PUBLIC)
      { strcat(buffer,"p"); } else APPEND_WS;
  if (flags & TYPE_MOD_VARARGS)
      { strcat(buffer,"V"); } else APPEND_WS;
  if (flags & TYPE_MOD_VIRTUAL)
      { strcat(buffer,"v"); } else APPEND_WS;
  if (flags & TYPE_MOD_PROTECTED)
      { strcat(buffer,"p"); } else APPEND_WS;
  strcat(buffer,"]");
 } 

struct function *locate_function( char *name, struct program *prog )
{
 // *** Sucht die Funktion mit dem Namen name im programm prog.
 int lauf1;
 int lauf2;  
 struct function *temp;  
   
 if (name == 0) 
   { 
    debug_message("*** DebugError: locate_function.name == 0 !\n");
    return 0;
   }
 if (prog->funktionsdaten == 0) 
   {
    debug_message("*** DebugError: locate_funtion.prog->funktionsdaten == 0 !\n");
    return 0;
   }
   
 for (lauf1 = 0; lauf1 < prog->num_functions ; lauf1++)
  {
   if (prog->funktionsdaten[lauf1].name == 0) 
    {
     debug_message("*** DebugError: locate_funtion.prog->funktionsdaten[lauf1].name == 0");
     return 0;
    }
     
   if (strcmp(prog->funktionsdaten[lauf1].name,name) == 0)
    { // *** Die Funktion hat den richtigen Namen !
     if (prog->funktionsdaten[lauf1].flags & NAME_INHERITED)
      { // *** MIST. Das Dings is inherited. Nun die Inherits durchsuchen.
       for (lauf2 = 0; lauf2 < prog->num_inherited; lauf2++)
	{
	 temp = locate_function(name,prog->inherit[lauf2].prog);
	 // *** Solange suchen, bis wir die richtige Funktion erwischen.
	 if (temp != 0)
	  { return temp; }
	}
      } else
      { // *** Da ham wir die Funktion. Zurueckspringen.
       return &(prog->funktionsdaten[lauf1]);
      }	
    }
  }
 return 0; 
}

struct svalue *get_global_variable_pointer( char *name )
 {
  // *** Gibt den zeiger auf die Svalue einer Objektglobalen Variablen
  // *** zurueck. Wenn eine Solche Variable nicht exisitert, wird 0
  // *** zurueckgegeben. 
  int lauf1;
  
  if ( name == 0 )
      { DEBERROR("*** DebugError: name == 0 !\n"); }
  
  for (lauf1 = 0; lauf1 < current_object->prog->num_variables; lauf1++)
   {
    if (current_object->prog->variable_names[lauf1].name == 0)
     {
      debug_message("*** DebugError: get_global_variable_pointer.c_o->p->v_n[l1].name == 0\n");
      return 0;
     }
    if (strcmp(current_object->prog->variable_names[lauf1].name,name) == 0)
     { // *** Gefunden !
//      return &(current_variables[lauf1]);
//       return &(csp->current_variables[lauf1]);
      return &(current_object->variables[lauf1]);
      // *** Gewagt, muesste aber funtkionieren !
     }
   }
  return 0;
 }

struct svalue *get_local_variable_pointer( char *name )
 {
  // *** Gibt den Zeiger auf die Svalue der lokalen Variable zurueck,
  // *** oder 0, wenn keine entsprechende lokale Variable gefunden werden konnte.
    
  // *** Dazu holen wir uns die aktuelle Funktion. 
  struct function *tempzeiger;
  int lauf1; 
  int lauf2;
    
  tempzeiger = locate_function( (char *) (*( (char **) (&csp->funstart[-5]) )),current_object->prog);

  if (tempzeiger == 0) return 0;
  // *** d.h. Funktion konnte nicht gefunden werden !
  if (tempzeiger->variable_names == 0) return 0;
  // *** So ein Aerger. Die Funktion hat garkeine lokalen Variablen.			       
			       
  lauf1 = -1;
  lauf2 = -1;
  do
   {
    lauf1++;
    if (strcmp(tempzeiger->variable_names[lauf1].name,name) == 0)
      { lauf2 = lauf1; }
   }
  while (tempzeiger->variable_names[lauf1].flags != 0xff);
 
   // *** Lauf2 -> Variable, lauf1 -> annzahl an Zeugs
   // *** lauf2 == -1 -> Variable nicht gefunden.
  if (lauf2 == -1) { return 0; }
  if (strcmp(tempzeiger->variable_names[lauf2].name,name) != 0) return 0;
			       
  // *** So, das Dingsbums ist eine lokale Variable.			      
  return &(inter_fp[lauf1-lauf2]);
  // *** Gewagt, aber es muesste funktionieren !
 }

void get_variable_data_for_svalue( char *buffer, struct svalue *variable )
 {
  int lauf1;
  char buf2[10000];
  struct hash_mapping *temphash;
  struct map_chain *temp_map_chain;
//  buffer[0] = 0x00;
  // *** Ganz langsam und ausfuehrlich !
  // *** Damit man es auch leicht verstehen kann, anders als den Rest des Drivers :(
    
#define ADDBUFFER(x)  strcat(buffer,x)
    
  strcat(buffer,"  Variablentyp : ");
  if (variable->type == T_INVALID)
      { ADDBUFFER(" Invalid (ups, wie kommt das hierher !?)"); } else
  if (variable->type == T_LVALUE)
      { ADDBUFFER("  LValue (ups, wie kommt das hierher !?)"); } else
  if (variable->type == T_NUMBER)
      { ADDBUFFER("  Number"); } else
  if (variable->type == T_STRING)
      { ADDBUFFER("  String"); } else
  if (variable->type == T_POINTER)
      { ADDBUFFER(" Pointer (d.h. ein Array)"); } else
  if (variable->type == T_OBJECT) 
      { ADDBUFFER("  Objekt"); } else
  if (variable->type == T_MAPPING)
      { ADDBUFFER(" Mapping"); } else
  if (variable->type == T_FLOAT)
      { ADDBUFFER("   Float"); } else
  if (variable->type == T_CLOSURE)
      { ADDBUFFER(" Closure"); } else
  if (variable->type == T_SYMBOL)
      { ADDBUFFER("  Symbol"); } else
  if (variable->type == T_QUOTED_ARRAY)
      { ADDBUFFER(" Q-Array"); }
    
  ADDBUFFER("\n");
  if (variable->type == T_NUMBER)
   {
    sprintf(buf2,"        Wert : %d\n",variable->u.number);
    ADDBUFFER(buf2);
    if (variable->u.number == 0)
     { 
      ADDBUFFER("Achtung !\nDieser Wert entspricht auch der \"uninitialisiert\" bzw. \"leer\"-Notation !\n"); 
     }
   } else
  if (variable->type == T_STRING)
   {
    sprintf(buf2,"          Wert : %s\n",variable->u.string);
    ADDBUFFER(buf2);   
   } else
 if (variable->type == T_FLOAT)
  {
    sprintf(buf2,"        Wert : %g\n",READ_DOUBLE(variable));
    ADDBUFFER(buf2);
  } else
 if (variable->type == T_POINTER)
  {
   sprintf(buf2," Anzahl der Elemente : %d\n",VEC_SIZE(variable->u.vec));
   ADDBUFFER(buf2);
   sprintf(buf2,"Dump der Variablen : \n");
   ADDBUFFER(buf2);
   for (lauf1 = 0; lauf1 < VEC_SIZE(variable->u.vec) ; lauf1++)
    {
     sprintf(buf2,"Dump von Element %d der Variablen :\n",lauf1);
     ADDBUFFER(buf2);
     get_variable_data_for_svalue(buffer,&(variable->u.vec->item[lauf1]));
    }
  } else
 if (variable->type == T_MAPPING)
  { // *** Anm. Mappingausgabe is noch nicht so doll und sehr evtl. fehlerhaft ! */
   sprintf(buf2," Reference-Count : %d\n",variable->u.map->ref);
   ADDBUFFER(buf2);
   temphash = variable->u.map->hash;

   // *** Nun die einzelnen Hash-Tabellen durchgehen. 
   for (lauf1 = 0; lauf1 < temphash->used; lauf1++)
    {
     temp_map_chain = temphash->chains[lauf1];
     // *** Tempmapchain zeigt jetzt auf das erste Element der jeweiligen
     // *** hashkette.
     while (temp_map_chain != 0)
      {
       sprintf(buf2," Mapping-Key : \n");
       ADDBUFFER(buf2);
       get_variable_data_for_svalue(buffer,&(temp_map_chain->key));
       ADDBUFFER("\n");
       sprintf(buf2," Mapping-Wert : \n");
       ADDBUFFER(buf2);
       get_variable_data_for_svalue(buffer,&(temp_map_chain->data));
       temp_map_chain = temp_map_chain->next;
       ADDBUFFER("\n\n");
      }
    }
  } else
 if (variable->type == T_OBJECT)
  {
    if (variable->u.ob == 0)
     { ADDBUFFER("Interner Fehler - Objektreferenz ungueltig !\n"); return; }
    if (variable->u.ob->name == 0)
     { ADDBUFFER("Interner Fehler - Kein referenzierter Objektname !\n"); return; }
	
    sprintf(buf2," referenziertes Objekt : %s\n",variable->u.ob->name);
    ADDBUFFER(buf2);
    ADDBUFFER("                 Flags : ");
    /* Das per Hand machen, wird sonst nirgends benoetigt. */
    if (variable->u.ob->flags & O_HEART_BEAT)
      ADDBUFFER("[Heartbeat]");
    if (variable->u.ob->flags & O_IS_WIZARD )
      ADDBUFFER("[Wizard]");
    if (variable->u.ob->flags & O_ENABLE_COMMANDS )
      ADDBUFFER("[Enable Commands]");
    if (variable->u.ob->flags & O_CLONE)
      ADDBUFFER("[Clone]");      
    if (variable->u.ob->flags & O_SHADOW)
      ADDBUFFER("[Shadow]");
      
    ADDBUFFER("\n");
    sprintf(buf2,"                 Licht : %d\n",variable->u.ob->total_light); 
    ADDBUFFER(buf2);
    sprintf(buf2,"       naechster Reset : %d\n",variable->u.ob->next_reset);
    ADDBUFFER(buf2);
       ADDBUFFER("              Enthaelt : ");
   if (variable->u.ob->contains != 0)
	{ ADDBUFFER(variable->u.ob->contains->name); } else
	{ ADDBUFFER("Nix, null, nada !"); }
   ADDBUFFER("\n");
       ADDBUFFER("              Umgebung : ");
   if (variable->u.ob->super != 0)
        { ADDBUFFER(variable->u.ob->super->name); } else
	{ ADDBUFFER("Nix, null, nada !"); }
    ADDBUFFER("\n");
    if (variable->u.ob->user != 0)
     {
      sprintf(buf2,"         Programmierer : %s\n",variable->u.ob->user->name);
      ADDBUFFER(buf2);
     } else ADDBUFFER("         Programmierer : Keiner, Niemand, anonym !\n");
    if (variable->u.ob->eff_user != 0)
     {
      sprintf(buf2,"         Effektive UID : %s\n",variable->u.ob->eff_user->name);
      ADDBUFFER(buf2);
     } else ADDBUFFER("         Effektive UID : Keine, nix, leer, nada !\n");
   } else
   {
    ADDBUFFER("Tut mir leid. Dieser Variablentyp kann noch nicht dargestellt werden !\nErinnere Sunblade mal, das es das nit vergisst !\n");
   }
  }
									      
struct breakpoint *breakpoint_exists( int curr_line, char *curr_file )
 {
  // *** Diese Funtkion ueberpreuft, ob ein Breakpoint existiert,
  // *** der an der Zeile curr_line in der Datei curr_file gesetzt ist.
  struct breakpoint *tempbreak;
    
  tempbreak = first_breakpoint;
  while (tempbreak != 0) 
   {
    if ((curr_line == tempbreak->line) &&
	(strcmp(curr_file,tempbreak->in_file) == 0))
     { // *** Da ist ein Breakpoint gefunden worden !
      return tempbreak;
     }
    tempbreak = tempbreak->next;
   }
  return 0;
 }
     

										    
