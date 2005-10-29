
//File defines
#define GEN_FERRY "/objects/ships/ferry/ferry"
#define FERRY_ROOMS "/objects/ships/ferry/"

#define AT_DW ({"announce arrive darkwind","hold 5",\
      "announce warning darkwind","hold 2","announce leave darkwind" })

#define AT_SV ({"announce arrive souvrael","hold 5",\
      "announce warning souvrael","hold 2","announce leave souvrael" })

#define AT_HB ({"announce arrive hyperborea", "hold 5",\
      "announce warning hyperborea","hold 2","announce leave hyperborea"})

#define AT_OD ({"announce arrive kerei","hold 5",\
      "announce warning kerei", "hold 2","announce leave kerei" })

#define DW_TO_GEARNAT ({"push","hoist","east","north","north","north","north",\
  "north","north","east","north","north","northeast","east"})

#define SALMO_TO_SV ({"east","south","south","south","south","south","south",\
  "south","south","south","south","south","south","south","south","south",\
  "south","south","south","south","south","south","south","south","south",\
  "south","south","south","south",\
  "east","east","east","east","east","east","east","east","east","east",\
  "east","east","east","east","east","east","east","east","east","east",\
  "east","east","east","east","east","east","east","east","east","east",\
  "east","east","east","east","east","east","east","east","east","east",\
  "east","east","east","east","east","east","east","east","east","furl","dock"})

#define SALMO_TO_AEGIR ({"east","east","east","north","north","north","north",\
  "north","north","north","east","east","east","east","east","east","east",\
  "east","east","east","east","east","east","east","east","east","east","east",\
  "east","north","north"})


#define SV_TO_SALMO ({"push","hoist","west","north","north","north","north","north","north",\
  "north","north","north","north","north","north","north","north","north",\
  "north","north","north","north","north","north","north","north","north",\
  "north","north","north","north",\
  "west","west","west","west","west","west","west","west","west","west",\
  "west","west","west","west","west","west","west","west","west","west",\
  "west","west","west","west","west","west","west","west","west","west",\
  "west","west","west","west","west","west","west","west","west","west",\
  "west","west","west","west","west","west","west","west","west"})

#define AEGIR_TO_SALMO ({"south","south","west","west","west","west","west",\
  "west","west","west","west","west","west","west","west","west","west",\
  "west","west","west","west","south","south","south","south","south","south",\
  "south","west","west","west"})

#define GEARNAT_TO_FJORD ({"north","north","north","north","north",\
  "west","north","west","north","west","north","west","north","west","north",\
  "west","north","west","north","west","north","west","north","west","north",\
  "west","north","west","north","west","north","west","north","west","north",\
  "west","north","west","north","west","north","west","north"})

#define GEARNAT_TO_DW ({"west","southwest","south","south","west","south","south",\
  "south","south","south","south","west","furl","dock"})

#define FJORD_TO_HB ({"northwest","northwest","west","west","furl","dock"})

#define FJORD_TO_GEARNAT ({"south","east","south","east",\
  "south","east","south","east","south","east","south","east","south","east",\
  "south","east","south","east","south","east","south","east","south","east",\
  "south","east","south","east","south","east","south","east","south","east",\
  "south","east","south","east","south","south","south","south","south"})

#define HB_TO_FJORD ({"push","hoist","east","east","southeast","southeast"})

#define SV_TO_RIVER ({"push","hoist","west","west","south","south","south",\
      "south","south","south","south","south","south","south","south",\
      "south","south","east"})

#define RIVER_TO_OD ({"south","south","south","south","south","furl","dock"})

#define OD_TO_RIVER ({"push","hoist","north","north","north","north","north"})

#define RIVER_TO_SV ({"west","north","north","north","north","north","north",\
       "north","north","north","north","north","north","north","east",\
       "east","furl","dock"})
