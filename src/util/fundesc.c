#include <stdio.h>
#ifndef M_UNIX
#include <strings.h>
#endif

/*
  Process stdin, dir to write doc files in argv[1]
  argv[2] (optional) is the name of the processed file.

*/

char buf[2000];
char *start = "* Description:";
char *stend = "{";
char *dfile;
int line_count;

/* Search string in backwards
*/
char *backstrchr (apa, ch)
    char *apa;
    char ch;
{
  char *sp;

  sp = apa;
  while (*sp) {
    if (*sp==ch) return sp;
    sp--;
  }
  return 0;
}

void
make_desc(fdesc)
     char *fdesc;

{
  char *tmp, *tmp2, *tmp3, *tmp4, fname[100];
  int n,il;
  FILE *f;

  tmp = (char *) strchr(fdesc,'p');
  while (tmp)  {
    if (strncmp(tmp,"public",6) == 0) break;
    tmp = (char *) strchr(&tmp[1],'p');
  }
  if (!tmp) return;
  *tmp = 0; tmp+=6;
  tmp2 = (char*) strchr(tmp,'('); if (!tmp2) return;
  tmp3 = (char*) backstrchr(tmp2,' ');
  tmp4 = (char *) backstrchr(tmp2,'\n');
  if (!tmp3) {
      tmp3 = tmp4;
      if (!tmp3) return;
  }
  else if ((tmp4) && (tmp3<tmp4))
      tmp3 = tmp4;
  *tmp2 = 0; strcpy(fname,&tmp3[1]);  *tmp2='(';

  if (f=fopen(fname,"a"))
      if (dfile)
	  fprintf(f,"%s\n/*\n * Source: %s\n * Line: %d\n *\n * Description:%s\n\n",
	      tmp,dfile,line_count-1,fdesc);
      else
	  fprintf(f,"%s\n/*\n * Line: %d\n *\n * Description:%s\n\n",
	      tmp,line_count-1,fdesc);
  fclose(f);
}


int
main(argc, argv)
     int argc;
     char **argv;
{
    int pflag, ch;
    char *tmp, *bpek;

    if (argc<2) {
	perror("To few args");
	exit(1);
    }

    if (chdir(argv[1])) {
	perror("Can't change to doc dir\n");
	exit(1);
    }

    if (argc > 2)
	dfile = argv[2];
    else
	dfile = 0;

    pflag = 0; tmp = start; bpek = buf; line_count = 1;

    while ((ch=getchar()) != EOF) {

	if (ch == '\n')
	    line_count ++;

	if (pflag) {
	    if (ch == *tmp) {
		tmp++; if (*tmp == 0) {
		    pflag = 0;
		    tmp = start;
		    *bpek = 0;
		    make_desc(buf);
		    bpek = buf;
		}
	    }
	    else {
		tmp = stend;
		*bpek = ch; bpek++;
	    }
	}
	else if (ch == *tmp) {
	    tmp++; if (*tmp == 0) {
		pflag = 1; tmp = stend;
	    }
	}
	else tmp = start;
    }
}

