#define MAKE_DOORS( loc_1, dir_1, loc_2, dir_2, lock_type, lock_code, door_long, is_closed, is_locked, can_lock) \
obj_1 = clone_object( "obj/door");\
obj_2 = clone_object( "obj/door");\
call_other( obj_1,"set_all",loc_1+" "+dir_1+" "+lock_type+" "+lock_code+" "+\
	   is_closed+" "+is_locked+" "+can_lock);\
call_other( obj_2,"set_all",loc_2+" "+dir_2+" "+lock_type+" "+lock_code+" "+\
	   is_closed+" "+is_locked+" "+can_lock);\
call_other( obj_1,"set_door",obj_2);\
call_other( obj_2,"set_door",obj_1);\
call_other( obj_1,"set_door_long",door_long);\
call_other( obj_2,"set_door_long",door_long);

#define MAKE_KEY( obj_variable, key_type, key_code) \
obj_variable = clone_object( "obj/key"); \
call_other( obj_variable, "set_key_data", key_type + " " + key_code);



