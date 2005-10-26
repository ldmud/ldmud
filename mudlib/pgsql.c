// Example object for the use of PostgreSQL efuns.

inherit "/i/item";
inherit "/i/move";

#define TP(x) player->tell(x)
//#define TP(x) this_player()->tell(x)
#include <input_to.h>
#include <pgsql.h>
#include <lpctypes.h>

int connected = 0;
object player;

string *keyvalues(mapping m, string key) {
   string *ret = ({});
   int i, rows = widthof(m);
   
   for (i = 0; i < rows; i++) {
      ret += ({ m[key,i] });
   }
   
   return ret;
}

string *prettytable (mixed res) {
   int rows, cols, i, j, total, tmp;
   string row;
   string *ret, *fnames, line, *fsizes;
   
   
   ret = ({});
   fsizes = ({});
   cols = sizeof(res);

   if (typeof(res) == T_POINTER) {
      for (i = 0; i < cols; i++)
	ret += ({ implode(res[i], ",") });
      return ret;
   }
   
   fnames = m_indices(res);
   rows = widthof(res);
   for (i = 0; i < cols; i++) {
      tmp = max(map(keyvalues(res, fnames[i])+({ fnames[i] }), #'sizeof));
      total += tmp;
      fsizes += ({ to_string(tmp) });
   }
   total += sizeof(fsizes)*3 + 1;
   
   ret += ({ sprintf(" +%"+to_string(total-2)+"'-'s+", "") });
   
   line = " | ";
   for (j = 0; j < cols; j++) {
      line += sprintf("%|"+fsizes[j]+"."+fsizes[j]+"s | ", fnames[j]);
   }
   ret += ({ line });
   ret += ({ ret[0] });
   for (i = 0; i < rows; i++) {
      line = " | ";
      for (j = 0; j < cols; j++) {
	 line += sprintf("%-"+fsizes[j]+"."+fsizes[j]+"s | ",
			 res[fnames[j],i]);
      }
      ret += ({ line });
   }
   ret += ({ ret[0] });
   
   return ret;
}

int result (int nr, mixed res, int id) {
   string *table;
   
   TP("\n");
   switch (nr) {
    case PGRES_COMMAND_OK:
      TP(res+"\n");
      break;
    case PGRES_TUPLES_OK:
      if (res) {
	 table = prettytable(res);
	 table += ({ "Rows: "+to_string(widthof(res))+"\n" });
	 TP(implode(table, "\n"));
      } else {
	 TP("No rows matched.\n");
      }
      break;
    case PGRES_BAD_RESPONSE:
    case PGRES_NONFATAL_ERROR:
    case PGRES_FATAL_ERROR:
      TP(res);
      break;
    case PGRES_NOTICE:
      TP(res);
      break;
    case PGCONN_SUCCESS:
      TP("PG-connection established.\n");
      connected = 1;
      break;
    case PGCONN_FAILED:
      TP("PG-connection failed: "+res+"\n");
      connected = 0;
      break;
    case PGCONN_ABORTED:
      TP("PG-connection lost: "+res+"\n");
      connected = 0;
      break;
   }
   if (connected)
     tell_object(player, "SQL> ");
   else
     tell_object(player, "sql> ");
}


int _sql (string str) {
   string prompt;
   
   if (str && !connected && (lower_case(str[0..6]) == "connect")) {
      str = str[8..];
      if (pg_connect(str, "result") < 0)
	TP("Error in connect-string");
      else
	TP("Connecting...");
   } else if (str && connected && (lower_case(str) == "disconnect")) {
      pg_close();
      connected = 0;
      TP("Disconnected...");
   } else if (str && ((lower_case(str) == "exit") || (lower_case(str) == "quit")))
     return 1;
   else if (str && strlen(str)) {
      if (!connected) 
	TP("Connect first, brainy\n");
      else
	pg_query(str, RESULT_ASSOC);
   }

   if (connected)
     prompt = "SQL> ";
   else
     prompt = "sql> ";
   
   input_to("_sql", INPUT_PROMPT, prompt);
   return 1;
}

int sql (string str) {
   TP("SQL-Shell v0.1a");
   TP("===============");
   TP("Commands: connect, disconnect, exit, quit");
   TP("Everything else is treated as SQL-command");
   TP("");
   _sql(str);
   return 1;
}

void init (void) {
   ::init();
   player = this_player();
   if (playerp(environment()))
     add_action("sql", "sql");
}

void create (void) {
   ::create();
   set_name("sqlshell");
   set_id(({"sqlshell","sql"}));
   set_long("Eine SQL-Shell");
   set_gender("w");
}
