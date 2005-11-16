// File:        /p/Registry/registry.c
// Description: erlaubt Kontrolle wo Inherits/Objekte genutzt werden
// Author:      Tucita (25.11.97)
// Last update: 27.4.99
// Modified by: Anin 28.08.01 auf umstrukturiertes P und ACLs umgestellt
//                            send_message, add_actions mit neg flags,
//                            sicherheitsabfrage bei loeschen/resetten rein
//              Anin 07.09.01 reset/delete id auslesen falls pfad angegeben
//              Anin 27.11.01 nosave bei vorgeschlagener dummy-variable dazu
//              Anin 07.12.01 lower_case raus weil bloedsinn zB bei /Tool/

/*    Die P - Registry

   Was ist die Registry?
   Wolltest du schon immer mal wissen wo deine tollen Objekte aus dem /p
   eingesetzt werden? Willst du ein Objekt verschieben, umbenennen oder gar
   loeschen, weisst aber nicht ob es jemand ausser dir selber nutzt? Die
   Antwort auf deine Fragen liegt hier. Die Registry verzeichnet die Files,
   die deine ueberwachten Objekte aufrufen - so weisst du wo deine Kreationen
   genutzt werden.

   Wie benutze ich die Registry?
   Einfach im create() des zu ueberwachenden Objektes ein
    "/p/Registry/registry"->register();
   einbauen - das wars! Und dann von Zeit zu Zeit in der Registry in die
   Listen schauen...

   Probleme:
   1. Hat das Objekt kein create() (z.B. Inherits, die keine Initialisierung
   benoetigen), so muss der Aufruf an eine Stelle, die garantiert nur einmal
   aufgerufen wird - und zwar beim Laden/Clonen des neuen Objektes.

   Loesung: * NEU *
   Benutze eine von einer Funktion initialisierte globale Variable. Diese
   Funktion wird garantiert von __INIT() automatisch aufgerufen - pro neu
   erzeugtem Objekt! Also:

   private int register()
   {
      seteuid(getuid()); // nicht vergessen!
      "/p/Registry/register"->register(__FILE__);
   }
   private nosave int dummy = register();

   ACHTUNG!! __INIT() selbst nicht mehr verwenden - geht auch nicht mehr...

   2. Ist das Objekt ein Inherit kann der Name des Inherits nur in diesem
   selber per __FILE__ festgestellt werden.

   Loesung:
   Als Parameter fuer den Aufruf __FILE__ angeben. Problem: Das ist nicht
   faelschungssicher.


   Fehler/Verbesserungen/Vorschlaege bitte an Tucita
*/

static variables inherit "/i/room";

#include <apps.h>
#include <level.h>
#include <more.h>
#include <message.h>
#include <acl.h>
#include <config.h>

#include "/p/Tool/sys/debugger.h"

// ein paar Standard-Defines
#define TO this_object()
#define TP this_player()
#define TI this_interactive()
#define PO previous_object()
#define ENV(x) environment(x)

#define SAVE_FILE "/p/Registry/registry"

#define CHANGED changed = 1; if (find_call_out("save")==-1) call_out("save",0)
#define SECURE(x) if (!(touch("/secure/master/")->query_acl_entry(x+".c",\
                                                       TP->query_real_name(),\
                                                       ACL_WRITE)) \
                   && member(FILED->query_auth("p"), \
                       TP->query_real_name())==-1 \
                   && !adminp(TP)) \
                     { send_message_to(TP,MT_NOTIFY,MA_USE,\
                                       "Du darfst das nicht.\n"); return 1; }

// Bestandteile des Registermappings
#define R_ID    0
#define R_CODER 1
#define R_TIME  2
#define R_CALLS 3

// reg = (["fname" : id; coder; time; use])
// use = (["caller" : count])
mapping register;
int     next_id  = 1,
       *free_ids = ({});

static status changed;

private int alloc_id();

/* der Aufruf:
   "/p/Registry/registry"->register();         fuer Objekte
   "/p/Registry/registry"->register(__FILE__); fuer Inherits
*/
varargs void register(string fname)
{                    // file name des ueberwachten Objektes
   string cname;     //  "    "    "  ladenden      "
   string name, map, *zerlegt;
   int x,y,dx,dy,*dc;

   DEBUG("P:REGIS",sprintf("1    FNAME: %s\n",fname||"-"));
   if (fname)
   {
     if (member(inherit_list(PO), fname)==-1)
         return;

      fname = fname[0..<3];
      if (!strstr(cname = object_name(PO), fname))
         cname = object_name(previous_object(1));
   DEBUG("P:REGIS",sprintf("2    CNAME: %s\n     FNAME: %s\n",
                           cname||"-",fname||"-"));
   }
   else
   {
      fname = object_name(PO);
      cname = object_name(previous_object(1));
      if (!strstr(cname, "/obj/player#"))    // fuer sinnvolle Aussagen
         cname = Name(previous_object(1));   // bei Autoloadern
   DEBUG("P:REGIS",sprintf("2b   CNAME: %s\n     FNAME: %s\n",
                           cname||"-",fname||"-"));
   }

   if (strstr(fname, "/p/")                // nur Objekte aus /p
    || !strstr(cname, "/obj/zauberstab")) // Direktaufrufe rausfiltern
      return;

   fname=explode(fname,"#")[0];
   if (!member(register, fname))
      register+=([fname : alloc_id();              // R_ID
                          explode(fname,"/")[2];   // R_CODER
                          time();                  // R_TIME
                          ([])                     // R_CALLS
                ]);
   DEBUG("P:REGIS",sprintf("3    CNAME: %s\n     FNAME: %s\n"
                           "     MAP2D1: %s\n     MAP2D: %s\n",
                           cname||"-",fname||"-",
                           map2domain(cname,1)||"-",map2domain(cname)||"-"));
   if(name=map2domain(cname,1)) {
       // es ist ein Map-Raum...
       name = (name[<2..<1]==".c") ? name[0..<3] : name;
       if(!map2domain(cname)) {
           // ...und es gibt kein File zu:
           DEBUG("P:REGIS",sprintf("4    NAME: %s\n",name||"-"));
           zerlegt=explode(name,"/");
           if(sizeof(zerlegt)>2) {
               map = implode(zerlegt[0..<2],"/")+"/map";
               if(file_size(map+".c") > 0) {
                   cname=map;
                   DEBUG("P:REGIS",sprintf("5    CNAME: %s\n",cname||"-"));
               }
           } else {
               cname="/map/map";
               DEBUG("P:REGIS",sprintf("5b   CNAME: %s\n",cname||"-"));
           }
           DEBUG("P:REGIS",sprintf("6    CNAME: %s\n",cname||"-"));
       } else {
           cname=name;
           DEBUG("P:REGIS",sprintf("4-6b CNAME: %s\n",cname||"-"));
       }
   } else {
       cname=explode(cname,"#")[0];
       DEBUG("P:REGIS",sprintf("4-6c CNAME: %s\n",cname||"-"));
   }
   if (!member(register[fname,R_CALLS], cname))
      register[fname,R_CALLS]+=([cname:1]);
   else
      register[fname,R_CALLS][cname]++;

   CHANGED;
}

// interna --------------------------------------------------------------------
private void load()
{
   if (file_size(SAVE_FILE+".o")>0)
      restore_object(SAVE_FILE);
   else
      register = ([]);

   changed = 0;
}

static void save()
{
   if (changed)
   {
      save_object(SAVE_FILE);
      changed = 0;
   }
}

private int alloc_id()
{
   int id;

   if (sizeof(free_ids))
   {
      id = free_ids[0];
      free_ids -= ({id});
   }
   else
      id = next_id++;

   CHANGED;
   return id;
}

private void free_id(int id)
{
   if (id>=1)
   {
      if (id==next_id-1)
         while (member(free_ids, (--next_id)-1)!=-1)
            free_ids -=({next_id-1});
      else if (id>=next_id || member(free_ids, id)!=-1)
         return;
      else
         free_ids += ({id});

      CHANGED;
   }
}

// standard functions ---------------------------------------------------------
string tafel_long()
{
    return
" +------------------------------------------------------------------------+\n"
" | Es gibt folgende Kommandos:                                            |\n"
" |                                                                        |\n"
" | nanu             - ausfuehrliche Erklaerung hierzu                     |\n"
" | liste            - Liste aller ueberwachten Objekte                    |\n"
" | liste <subdir>   - Liste der ueberwachten Objekte unter /p/<subdir>/   |\n"
" | detail <file|id> - Liste der Loader von Objekt <file> bzw <id>         |\n"
" | reset <file|id>  - setzt die Ladezaehler fuer Objekt <file> zurueck    |\n"
" | delete <file|id> - loescht Objekt <file> aus den Listen                |\n"
" |........................................................................|\n"
" |                                                                        |\n"
" | OBJEKTE / SHADOWS   \"/p/Registry/registry\"->register();                |"
"\n"
" | INHERITS            \"/p/Registry/registry\"->register(__FILE__);        |"
"\n"
" |                                                                        |\n"
" | OHNE CREATE                                                            |\n"
" |    private int register_im_p()                                         |\n"
" |    {                                                                   |\n"
" |       seteuid(getuid()); // nicht vergessen!                           |\n"
" |       \"/p/Registry/registry\"->register(__FILE__);                      |"
"\n"
" |    }                                                                   |\n"
" |    private nosave int dummy_p = register_im_p();                       |\n"
" +------------------------------------------------------------------------+"
"\n";
}

void create()
{
    seteuid(getuid());

    set_short("In einem grossen Register");
    set_long(
"          _______________\n"
"         /              /\n"
"       _/______________//  Du stehst mitten im Amt fuer Registrierung\n"
"      /(_nanu_________(//| der Nutzung von Objekten und Inherits im P.\n"
"     /_(__liste____(/__//\n"
"    (____reset________(/ \\ Um dich herum stapeln sich Berge von Akten\n"
"    _/|=============)'|  / und Listen.\n"
"   / ==/___________/ /  /\n"
"   ====|__detail___|/  /   Eine grosse, wichtige Tafel haengt an der Wand.\n"
"   /|             |\\  /\n"
"  / | delete      | \\/\n"
"    |_____________|/\n");
    set_own_light(1);
    add_type("kunstlicht",1);
    add_type("nocleanup",1);
    set_room_domain("Pantheon");
    set_exit("/p/Doc/room/halle","hoch");
    add_v_item(([
         "name" : "tafel",
       "gender" : "weiblich",
     "adjektiv" : ({"gross"}),
         "long" : #'tafel_long,
         "read" : #'tafel_long,
                ]));
    load();
}

void init()
{
   ::init();

   add_action("cmd_doku","nanu");
   add_action("cmd_liste","listen",-4);
   add_action("cmd_detail","details",-4);
   add_action("cmd_reset","resette",-4);
   add_action("cmd_delete","delete",-4);
   add_action("cmd_delete","loesche",-6);
}

int remove()
{
   save();
   return ::remove();
}

int query_prevent_shadow()
{
   return 1;
}

// user commands --------------------------------------------------------------
int cmd_doku()
{
   TP->more(({
"  nanu             - diese Erklaerung",
"  liste            - Liste aller ueberwachten Objekte",
"  liste <subdir>   - Liste der ueberwachten Objekte unter /p/<subdir>/",
"  detail <file|id> - Liste der Loader von Objekt <file> bzw <id>",
"  reset <file|id>  - setzt die Ladezaehler fuer Objekt <file> zurueck",
"  delete <file|id> - loescht Objekt <file> aus den Listen",
"",
"Was ist die Registry?",
"Wolltest du schon immer mal wissen wo deine tollen Objekte aus dem /p",
"eingesetzt werden? Willst du ein Objekt verschieben, umbenennen oder gar",
"loeschen, weisst aber nicht ob es jemand ausser dir selber nutzt? Die",
"Antwort auf deine Fragen liegt hier. Die Registry verzeichnet die Files, die",
"deine ueberwachten Objekte aufrufen - so weisst du wo deine Kreationen",
"genutzt werden.",
"",
"Wie benutze ich die Registry?",
"Du musst lediglich einen einzigen Aufruf in dein File einbauen:",
"   \"/p/Registry/registry\"->register();         fuer Objekte",
"   \"/p/Registry/registry\"->register(__FILE__); fuer Inherits",
"",
"Dieser Aufruf sollte ins create() deines Files. Hat es kein create(), weil",
"es ein Shadow ist, dann in die Funktion, die nach dem Clonen zuerst",
"aufgerufen wird. Diese heisst oft setup_shadow oder aehnlich.",
"Ohne create hat man auch die Moeglichkeit eine globale Variable, die von",
"einer Funktion initialisiert wird zu benutzen. (Diese wird fuer jedes neu",
"erzeugte Objekt automatisch von __INIT() aufgerufen.) Also etwa so:",
"",
"   private int register_im_p()",
"   {",
"      seteuid(getuid()); // nicht vergessen!",
"      \"/p/Registry/registry\"->register(__FILE__);",
"   }",
"   private nosave int dummy_p = register_im_p();",
}), 0, 0, M_AUTO_END);
   return 1;
}

int cmd_liste(string s)
{
    string *out;

    notify_fail("Liste oder liste <subdir>.\n");
    if (!s) {
        out = ({"Liste aller ueberwachten Files:",
                "Index   Filename"})
              +map(sort_array(m_indices(register), #'>),
                   (: return sprintf("%3d     %s",
                                     $2[$1,R_ID],$1); :),register);
    } else {
        s=strip(s);
        s = ((s[0..0]=="/") ? s[1..<1] : s);
        s = ((s[<1..<1]=="/") ? s[0..<2] : s);
        if(strstr(s,"/")!=-1) {
            notify_fail("Bisher kann man leider nur das 1. Subdir angeben!\n");
            return 0;
        }
        out = ({"Liste der Files in /p/"+s+"/",
                "Index   Filename"})
                +map(sort_array(filter(m_indices(register),
                                       (: return ($3==$2[$1,R_CODER]); :),
                                       register,s),
                                #'>),
                     (: return sprintf("%3d     %s",
                                       $2[$1,R_ID],$1); :),register);
        if (sizeof(out)==1) {
            return 0;
        }
    }
    TP->more(out, 0, 0, M_AUTO_END);
    return 1;
}

int cmd_detail(string fname)
{
    int id;
    mapping use, match;
    string *out;

    notify_fail("Detail <file> oder detail <id>.\n");
    if (!fname) {
        return 0;
    }
    if (!member(register, fname)) {
        if (sscanf(fname,"%d", id)!=1) {
           return 0;
        } else {
            match = filter(m_indices(register),
                           (: return $2[$1,R_ID] == $3; :), register, id);
           if (sizeof(match)!=1) {
              notify_fail("Ungueltige Id.\n");
              return 0;
           }
           fname = match[0];
        }
    } else {
        id = register[fname,R_ID];
    }
    use = register[fname,R_CALLS];
    out = ({"Detailanzeige fuer \""+fname+"\" (Id "+id+") unter "
            +register[fname,R_CODER],
            "Zaehlbeginn "+shorttimestr(register[fname,R_TIME]),
            "Zaehler Aufrufender"})
          +map(sort_array(m_indices(use), #'>),
               (: return sprintf("%4d    %s",$2[$1],$1); :),use);
    TP->more(out, 0, 0, M_AUTO_END);
    return 1;
}

int cmd_reset(string fname)
{
    mapping match;
    int id;

    notify_fail("Reset <file> oder reset <id>.\n");
    if (!fname) {
        return 0;
    }
    if (!member(register, fname)) {
        if (sscanf(fname,"%d", id)!=1) {
           return 0;
        } else {
            match = filter(m_indices(register),
                           (: return $2[$1,R_ID] == $3; :), register, id);
            if (sizeof(match)!=1) {
               notify_fail("Ungueltige Id.\n");
               return 0;
            }
            fname = match[0];
        }
    } else {
        id=register[fname,R_ID];
    }
    SECURE(fname)
    send_message_to(TP,MT_NOTIFY,MA_USE,
                    wrap("Moechtest du wirklich den Eintrag zu "+fname+
                         " resetten? (Ja/Nein)"));
    input_to("input_reset",0,fname,id);
    return 1;
}

int input_reset(string str, string fname, int id)
{
    if (lower_case(str) != "ja")
    {
        send_message_to(TP,MT_NOTIFY,MA_USE,"Ok, dann halt nicht.\n");
        return 1;
    }
    register[fname,R_TIME] = time();
    register[fname,R_CALLS] = ([]);
    send_message_to(TP,MT_NOTIFY,MA_USE,"Ok, Eintrag "+id+" resettet!\n");
    CHANGED;
    return 1;
}

int cmd_delete(string fname)
{
    mapping match;
    string coder;
    int id;

    notify_fail("Delete <file> oder delete <id>.\n");
    if (!fname) {
        return 0;
    }
    if (!member(register, fname)) {
        if (sscanf(fname, "%d", id)!=1) {
            return 0;
        } else {
            match = filter(m_indices(register),
                           (: return $2[$1,R_ID] == $3; :), register, id);
            if (sizeof(match)!=1) {
               notify_fail("Ungueltige Id.\n");
               return 0;
            }
            fname = match[0];
        }
    } else {
        id=register[fname,R_ID];
    }
    SECURE(fname)
    send_message_to(TP,MT_NOTIFY,MA_USE,
                    wrap("Moechtest du wirklich den Eintrag zu "+fname+
                         " loeschen? (Ja/Nein)"));
    input_to("input_delete",0,fname,id);
    return 1;
}

int input_delete(string str, string fname, int id)
{
    if (lower_case(str) != "ja")
    {
        send_message_to(TP,MT_NOTIFY,MA_USE,"Ok, dann halt nicht.\n");
        return 1;
    }
    m_delete(register,fname);
    free_id(id);
    send_message_to(TP,MT_NOTIFY,MA_USE,"Ok, Eintrag "+id+" geloescht!\n");
    CHANGED;
    return 1;
}

