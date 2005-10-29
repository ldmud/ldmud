#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_LIBC_H
#include <libc.h>
#endif
main(argc, argv)
    int argc;
    char **argv;
{
    int i, d, min;
    char *p, *q;
    int terse;
    int total = 12, negative = 0, min_index = 0;

    terse = argc > 1 && !strcmp(argv[1], "--terse");
    i = 0x8000;
    min = i;
    d = i;
    do {
	p = malloc(i);
	q = malloc(i);
	total++;
	d = q - (p+i);
	if (d <= min) {
	    if (d < 0) {
		negative++;
		if (negative*3 > total*2) {
		    fprintf(
		      stderr,
		      "Malloc returns does not return increasing addresses\n"
		    );
		    if (terse)
			printf("0");
		    exit(1);
		}
		continue;
	    }
	    if (d == min)
		break;
	    min = d;
	    min_index = i;
	    if (d == 0)
		break;
	}
	i -= sizeof(char *);
    } while (i >= 0x7f00);
    d = 0x8000 - min_index;
    if (d > 0x100 || min > 0x1000) {
	fprintf(stderr, "no credible value for EXTERN_MALLOC_OVERHEAD found\n");
	if (terse)
	    printf("0");
	fflush(stdout);
	exit(1);
    }
    printf(terse ? "%d" : "Suggested EXTERN_MALLOC_OVERHEAD: %d\n", d);
    if (!terse && d != min)
	printf("Actual overhead encountered for the above setting:\n", min);
}
