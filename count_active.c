#include <sys/types.h>
#include <sys/stat.h>

/*
 * Count the number of active players.
 * A players is supposed to be active, if he has logged in the last
 * 30 days.
 */

main(argc, argv)
    char **argv;
{
    struct stat st;
    int i, num;
    time_t now;

    now = time(0l);
    for (i=1, num=0; i<argc; i++) {
	if (stat(argv[i], &st) == -1)
	    continue;
	if (now - st.st_mtime < 30 * 24 * 60 * 60)
	    num++;
    }
    printf("%d\n", num);
}
