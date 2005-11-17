/*
 * This file defines flags that will make the game log facts.
 * If you change a definition, be sure to remove obj/player.i and
 * obj/living.i if you have them.
 */
/*
 * Log all set and clear of flags.
 * Log file: log/FLAGS
 */
#define LOG_FLAGS

/*
 * Define this flag if you want to log what object/player that
 * marks that a quest is solved.
 * Log file: log/QUESTS
 */
#define LOG_SET_QUEST

/*
 * Define this flag if there is to be a log of when players enter the game.
 * Log file: log/ENTER
 */
#define LOG_ENTER

/*
 * Define this flag if all new players are to be logged.
 * Log file: log/NEWPLAYER
 */
#define LOG_NEWPLAYER

/*
 * Define this flag if you want to log all shutdowns.
 * Log file: log/GAME_LOG
 */
#define LOG_SHUTDOWN

/*
 * Define this flag if you want to log when someone is killed, and by what.
 * Log file: log/KILLS
 */
#define LOG_KILLS

/*
 * Define this flag when you want to log illegal manipulations of the
 * player.c object.
 * Log file: log/ILLEGAL
 */
/* define LOG_ILLEGAL_PLAYER */

/*
 * Define this flag if you want to log everytime a player gets experience
 * points or money from an interactive player.
 * Log file: log/EXPERIENCE
 * ROOM_EXP_LIMIT should be defined to the least amount of exp to be logged.
 */
#define LOG_EXP
#define ROOM_EXP_LIMIT 10000
