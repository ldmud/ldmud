/*
 * This file is used to tune the game
 * The game should be "easy" in the beginning, because there are not
 * enough monsters and puzzles.
 * When wizards start to create new rooms, an inflation will be noted
 * (it is easier to become wizard).
 */

/*
 * This value is used to tune the cost to advance a stat in a player.
 */

#define STAT_COST 10

/*
 *
 * If you change this value, remove adv_guild.i (if you have it), so that
 * the new value will be used at restart of the game.
 *
 * EXP_COST: How many experience points you get for 1000 gold coins in
 * avdenturers guild.
 *	2000: For an easy start.
 *	1000: Normal.
 *	500:  Extremly inflated game.
 *
 * Big LPmuds usually have several guilds made by other wizards. It is
 * very important that they call query_cost() in adv_guild.c to determine
 * what the cost is to advance a player. All guilds should have the same
 * cost for advancing.
 */

#define EXP_COST	600

/*
 * When there are many quests in the game, it is not good to force
 * the players to solve all of them. FREE_QUESTS defines how many of
 * them that you don't have to solve. If you change this value, remove
 * quert_room.i (if you have one), so that the new value will be used
 * when the game is restarted next time.
 *
 */
/*
 * Recommended values:
 *
 * Number of quests		FREE_QUESTS
 * 1 (default)			0
 * 4				0
 * 8				2
 * 16				4
 * 24				6
 */

#define FREE_QUESTS	3
