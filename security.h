/* wizard level catagories */
#define SENIOR 100
#define ELDER 500
#define GOD 10000

/* standard security */
#define EXPLORE 20
#define CREATE 21
#define TITLE 22
#define INVIS 30
#define STAT 40
#define OUT_OF_BODY 50
#define CONTROL 50
#define SNOOP 60
#define DAMAGE 70
#define ITEM_OVER 90
#define ECHO 300
#define SHUTDOWN 600
#define ALL_POWER GOD

/* invisability */
#define NO_SHORT 10
#define CAN_NO_SHORT 30
#define NO_WHO 20
#define CAN_NO_WHO 35
#define SOMEONE 30
#define CAN_SOMEONE 40
#define INVIS_TELEPORT 40
#define CAN_INVIS_TELEPORT 60
#define INVIS_ACTION 50
#define CAN_INVIS_ACTION 100
#define NO_ID 60
#define CAN_NO_ID 300
#define NO_PEOPLE 100
#define CAN_NO_PEOPLE 600
/* to make life easier */
#define INV1 NO_SHORT
#define CINV1 CAN_NO_SHORT
#define INV2 NO_WHO
#define CINV2 CAN_NO_WHO
#define INV3 SOMEONE
#define CINV3 CAN_SOMEONE
#define INV4 INVIS_TELEPORT
#define CINV4 CAN_INVIS_TELEPORT
#define INV5 INVIS_ACTION
#define CINV5 CAN_INVIS_ACTION
#define INV6 NO_ID
#define CINV6 CAN_NO_ID
#define INV7 NO_PEOPLE
#define CINV7 CAN_NO_PEOPLE


/* file access */
#define ED_OTHERS   SENIOR
#define ED_LOG	    ELDER
#define READ_OTHERS STAT
