string type;
string code;


short()
{
 return "A " + type + " key";
}

set_key_data( str)
{
  if ( sscanf(str, "%s %s", type, code) == 2) 
    return 1;
  return 2;
}
long()
{
 write("\nThis a " + type + " key, wonder where it fits?\n");
}

id( strang)
{
 if ( ( strang == "key" )||( strang == type + " key")||( strang == "H_key") )
   return 1;
 return 0;
}

get()
{
  return 1;
}

query_value()
{
 return 10;
}

query_type() { return type; }
query_code() { return code; }

set_type( str) { type = str; }
set_code( str) { code = str; }

init()
{
}

reset( arg)
{
 if(arg)
   return;
 type = 0;
 code = 0;
}

