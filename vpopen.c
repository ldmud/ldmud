#include <sys/types.h>
#include <sys/signal.h>
#include <stdio.h>

#ifdef BSD
#define strchr(a,b) index(a,b)
#endif

/* Maximum size of command */
#define ARGSPACESIZE 1000

extern char *strchr();

static uid_t *pids;
static int fds;

FILE *
vpopen(program, type)
	char *program, *type;
{
	register char *cp;
	FILE *iop;
	int argc, gargc, pdes[2], pid;
	char argspace[ARGSPACESIZE];
	char *argv[15];

	if (*type != 'r' && *type != 'w' || type[1])
		return(NULL);

	if (!pids) {
		if ((fds = getdtablesize()) <= 0)
			return(NULL);
		if (!(pids =
		    (uid_t *)malloc((u_int)(fds * sizeof(uid_t)))))
			return(NULL);
		bzero(pids, fds * sizeof(uid_t));
	}
	if (pipe(pdes) < 0)
		return(NULL);

	/* break up string into pieces, put into argv, and \0 delimit */
	strncpy(argspace, program, ARGSPACESIZE);
	argv[0] = argspace;
	argc = 1;
	while (argv[argc] = strchr(argv[argc-1],' '))
	    *(argv[argc++]++) = '\0';

	iop = NULL;
	switch(pid = vfork()) {
	case -1:			/* error */
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		return iop;
		/* NOTREACHED */
	case 0:				/* child */
		if (*type == 'r') {
			if (pdes[1] != 1) {
				dup2(pdes[1], 1);
				(void)close(pdes[1]);
			}
			/* Try to get stderr piped as well. */
			pdes[2] = 1;
			(void)close(pdes[0]);
		} else {
			if (pdes[0] != 0) {
				dup2(pdes[0], 0);
				(void)close(pdes[0]);
			}
			(void)close(pdes[1]);
		}
		execv(argv[0], argv);
		_exit(1);
	}
	/* parent; assume fdopen can't fail...  */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void)close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void)close(pdes[0]);
	}
	pids[fileno(iop)] = pid;

	return iop;
}

vpclose(iop)
	FILE *iop;
{
	register int fdes;
	long omask;
	int pid, stat_loc;
	u_int waitpid();

	/*
	 * pclose returns -1 if stream is not associated with a
	 * `popened' command, or, if already `pclosed'.
	 */
	if (pids[fdes = fileno(iop)] == 0)
		return(-1);
	(void)fclose(iop);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	while ((pid = wait(&stat_loc)) != pids[fdes] && pid != -1);
	(void)sigsetmask(omask);
	pids[fdes] = 0;
	return(stat_loc);
}
