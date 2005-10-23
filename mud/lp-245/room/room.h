inherit "room/room";

#define EXTRA_RESET

#define ONE_EXIT(DEST, DIR, SH, LO, LIGHT)\
void reset(int arg) { if (!arg) { set_light(LIGHT); short_desc = SH;\
    long_desc = LO; dest_dir = ({ DEST, DIR }); } EXTRA_RESET }

#define TWO_EXIT(DEST1, DIR1, DEST2, DIR2, SH, LO, LIGHT)\
void reset(int arg) { if (!arg) { set_light(LIGHT);\
    short_desc = SH; long_desc = LO;\
    dest_dir = ({ DEST1, DIR1, DEST2, DIR2 }); } EXTRA_RESET }


#define THREE_EXIT(DEST1, DIR1, DEST2, DIR2, DEST3, DIR3, SH, LO, LIGHT)\
void reset(int arg) { if (!arg) { set_light(LIGHT);\
    short_desc = SH; long_desc = LO;\
    dest_dir = ({ DEST1, DIR1, DEST2, DIR2, DEST3, DIR3 }); } EXTRA_RESET }


#define FOUR_EXIT(DEST1, DIR1, DEST2, DIR2, DEST3, DIR3, DEST4, DIR4, SH, LO, LIGHT)\
void reset(int arg) { if (!arg) { set_light(LIGHT);\
    short_desc = SH; long_desc = LO;\
    dest_dir = ({ DEST1, DIR1, DEST2, DIR2, DEST3, DIR3, DEST4, DIR4 }); }\
    EXTRA_RESET }
