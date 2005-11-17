#include <stdio.h>
#include <string.h>

#define VERSION "0.16"


typedef struct errinfo {
  struct errinfo *next;
  int lnr;
  short type;
} errinfo;


typedef struct asmtype {
  short type;
#define ELSE    0
#define JMP_PC  1
#define BRA     2
#define BSR     3
#define BCC     4
  char  label[40];
  short cc;
} asmtype;


typedef struct todoinfo {  /* For modifications of the switch-table */
  struct todoinfo *next;
  char label[40];
  int  mylabel;
} todoinfo;


static char inbuf[10240], outbuf[10240];

  /* Branch conditions are paired ! */
static char *ccs[] = {"cc", "cs", "eq", "ne", "ge", "lt", "gt", "le"
                   , "hi", "ls", "mi", "pl", "vc", "vs"};
static char *asm, *newasm;
static errinfo *elist = NULL;
static todoinfo *todo = NULL;
static char *prefix;
static int result = 0;
static int mylabel = 1;
static int version = 0; /* 0: 2.06.xx, 1: 2.07.xx */

static insert_err (int lno, short t) {
  errinfo **ptr, *new;
  ptr = &elist;
  while (*ptr != NULL && ptr->next->lnr < lno) ptr = (errinfo **)ptr->next;
  new = (errinfo *) malloc (sizeof(errinfo));
  new->lnr = lno; new->type = t; new->next = *ptr;
  *ptr = new;
}


static void push_todo (char *l, int myl) { /* I know it's slow...*/
  todoinfo **ptr, *new;
  ptr = &todo;
  while (*ptr != NULL) ptr = (todoinfo **)ptr->next;
  new = (todoinfo *) malloc (sizeof(todoinfo));
  strcpy (new->label, l); new->mylabel = myl; new->next = *ptr;
  *ptr = new;
}


static clean_up() {
  void *help;
  while (elist != NULL) { help = (void *) elist; elist = elist->next; free(help); }
  while (todo != NULL) { help = (void *) todo; todo = todo->next; free(help); }
}


static void usage() {
  printf ("dstol <asmfile> <errfile> <new asmfile> [<prefix>]\n\n");
  printf ("dstol %s  --  Copyleft by Lars Düning\n\n", VERSION);
  printf ("Takes the result from a failed assembly and corrects the source\n");
  printf ("as long as the 'error' is just a switch() of more than 32 KByte.\n");
  printf ("There are chances that this will only work in patching 'interpret.c'\n");
  printf ("of Amylaar's LPMud-Driver...\n");
  printf ("The added labels are prefixed by <prefix>, default is 'x'.\n");
  exit (0);
}


static asmtype whatisit (char *line) {
  asmtype rc;
  char *parentp;

  rc.type = ELSE;
  if (!line[0]) return rc;
  strncpy (rc.label, line+5, 39); rc.label[39] = '\0';
  if (parentp = strchr (rc.label, '\n')) *parentp = '\0';
  if (parentp = strchr (rc.label, '(')) *parentp = '\0';
  line++;
  if (!strncmp(line, "jmp", 3) && parentp && !strncmp(parentp+1,"pc)",3))
    rc.type = JMP_PC;
  else if (!strncmp(line, "bra", 3) && !parentp)
    rc.type = BRA;
  else if (!strncmp(line, "bsr", 3) && !parentp)
    rc.type = BSR;
  else if (line[0] == 'b' && line[3] == '\t' && !parentp) {
    rc.type = BCC;
    for (rc.cc = 0; rc.cc < 14 && strncmp(line+1, ccs[rc.cc], 2); rc.cc++);
    if (rc.cc >= 14) rc.type = ELSE;
  }
  return rc;
}


  /* Read one line into buf and return buf, else NULL if eof is reached.
  ** If it's a comment or empty line, buf gets a null string.
  */
static char *getline (char *buf, int len, FILE *f) {
  int c;
  if ((c = getc(f)) == EOF) return NULL;
  if (!c || c == '*' || c == '\n') {
    buf[0] = '\0';
    while (c != '\n' && (c = getc(f)) != EOF);
  }
  else {
    buf[0] = c; buf[1] = '\0';
    if (fgets (buf+1, len-1, f) == NULL) return NULL;
  }
  return buf;
}


static int read_errfile (char *name) {
  char line[256], line2[256];
  char *cp;
  FILE *ferr;
  int lnr;

  if ((ferr = fopen (name, "r")) == NULL) {
    fprintf (stderr, "Can't open errfile '%s'\n", name);
    result = 20;
    return 0;
  }
  if (fgets (line, 255, ferr) == NULL) {
    fclose (ferr);
    return 1;
  }
  if (!strncmp (line, "DAS: Line", 9)) version = 0;
  else if (!strncmp (line, "DAS: \"", 6)) version = 1;
  else {
    fclose (ferr);
    fprintf (stderr, "Can't determine type of error file.\n");
    fprintf (stderr, "First line is: %s", line);
    result = 10;
    return 0;
  }

  do {
    switch (version) {
    case 0:
        if (sscanf (line,"DAS: Line %d", &lnr) != 1) lnr = -1;
        else {
          sprintf (line2, "DAS: Line %d Error      word offset too large\n", lnr);
          if (!strcmp(line, line2)) insert_err (lnr, 0);
          else {
            sprintf (
              line2,
              "DAS: Line %d Error      word branch offset too large\n",
              lnr);
            if (!strcmp(line, line2)) insert_err (lnr, 1);
            else lnr = -1;
          }
        }
        break;
    case 1:
        if (!(cp = strchr (line, '\"')) || !(cp = strchr (cp+1, '\"'))
          || sscanf (++cp, " L:%d", &lnr) != 1) lnr = -1;
        else {
          sprintf (line2, " L:%d Error:44 Word offset out of range:", lnr);
          if (!strncmp(cp, line2, strlen(line2))) insert_err (lnr, 0);
          else lnr = -1;
        }
        break;
    default:
        lnr = -1;
        break;
    }
    if (lnr < 0) {
        fclose (ferr);
        fprintf (stderr, "Unknown error msg: %s", line);
        fprintf (stderr, "Maybe you misunderstood this program?\n");
        result = 10;
        return 0;
    }
  }
  while (fgets (line, 255, ferr) != NULL);
  fclose (ferr);
  return 1;
}


int convert_file (char *in, char *out) {
  FILE *fin, *fout;
  char line[256];
  errinfo *ep;
  todoinfo *tp;
  asmtype asm;
  int lno, mtable;

  ep = elist; lno = 0;

  if ((fin = fopen (in, "r")) == NULL) {
    fprintf (stderr, "Can't open sourcefile '%s'\n", in);
    result = 20;
    return 0;
  }
  if ((fout = fopen (out, "w")) == NULL) {
    fprintf (stderr, "Can't open new sourcefile '%s'\n", out);
    fclose (fin);
    result = 20;
    return 0;
  }
  setvbuf (fin, inbuf, _IOFBF, sizeof(inbuf));
  setvbuf (fout, outbuf, _IOFBF, sizeof(outbuf));
  mtable = 0;
  while (ep != NULL && getline (line, 255, fin) != NULL) {
   lno++;
   if (mtable) { /* End of switch table-to-modify ? */
     asm = whatisit (line);
     if (asm.type != JMP_PC) {
       while (todo != NULL) {
         fprintf (fout, "%s%d\n\tjmp\t%s\n", prefix, todo->mylabel, todo->label);
         tp = todo; todo = todo->next; free(tp);
       }
     }
   }
   if (ep == NULL || lno != ep->lnr) {
     if (mtable && asm.type != JMP_PC && (!*line || *line != ';'))
       mtable = 0;
     if (*line) fputs (line, fout);
   }
   else {
     while (ep != NULL && lno == ep->lnr) {
        if (!mtable) asm = whatisit (line);
        else if (asm.type != JMP_PC) mtable = 0;
        switch (asm.type) {
        case JMP_PC: push_todo (asm.label, mylabel++);
                     fprintf (fout, "\tjmp\t%s%d(pc)\n", prefix, mylabel-1);
                     mtable = 1;
                     break;
        case BRA:
                     fprintf (fout, "\tjmp\t%s\n", asm.label);
                     break;
        case BSR:
                     fprintf (fout, "\tjsr\t%s\n", asm.label);
                     break;
        case BCC:
                     fprintf (fout, "\tb%s\t%s%d\n", ccs[asm.cc^0x01], prefix, mylabel++);
                     fprintf (fout, "\tjmp\t%s\n", asm.label);
                     fprintf (fout, "%s%d\n", prefix, mylabel-1);
                     break;
        default: fprintf ( stderr, "Oops, can't handle this one.\nLine %d: %s"
                         , lno, line);
                 result = 5;
         if (*line) fputs (line, fout);
                 break;
        }
        ep = ep->next;
      }
    }
  }

  /* No more errors pending, but there may still be a jump table
  ** to be completed: scan forward until table end, then insert
  ** the remaining code.
  */
  if (mtable) {
    while (mtable && getline (line, 255, fin) != NULL) {
     asm = whatisit (line);
     if (asm.type != JMP_PC) mtable = 0;
     else if (*line) fputs (line, fout);

    }
    while (todo != NULL) {
      fprintf (fout, "%s%d\n\tjmp\t%s\n", prefix, todo->mylabel, todo->label);
           tp = todo; todo = todo->next; free(tp);
    }
    if (!mtable && *line) fputs (line, fout); /* else the fgets had failed */
  }

  /* All patched, now just carry on copying...*/
  while (getline (line, 255, fin) != NULL) if (*line) fputs (line, fout);

  fclose (fout);
  fclose (fin);
  return 0;
}


int main (int argc, char **argv) {

  if (argc < 4 || argc > 5) usage();

  if (argc == 4) prefix = "x"; else prefix = *(argv+4);
  if (!read_errfile (*(argv+2))) goto byebye;
  if (!convert_file (*(argv+1), *(argv+3))) goto byebye;

byebye:
  clean_up();
  exit(result);
}
