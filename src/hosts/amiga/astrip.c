#include <stdio.h>
#include <string.h>
#define VERSION "0.6"
static char inbuf[10240], outbuf[10240];
static int result = 0;
static int redebug = 0;
static void usage() {
  puts("astrip <asmfile> <new asmfile> [redebug]\n");
  printf("astrip %s  --  Copyleft by Lars Düning\n\n", VERSION);
  puts("Takes the assembler source produces by a compiler and strips");
  puts("off all empty and commenting lines (i.e. lines starting with");
  puts("a ';' or a '*').");
  puts("'redebug' not given: 'debug' are commented out.");
  puts("'redebug' given: commented out 'debug' statements are reactivated.");
  exit (0);
}
int convert_file (char *in, char *out) {
  FILE *fin, *fout;
  char line[256];
  int c;
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

    /* The skiping of the commenting lines must be done charwise
    ** because the preprocessor/compiler generates lines of
    ** arbitrary length.
    ** The generated assembler lines are of limited length.
    */
  while ((c = getc(fin)) != EOF)
  {
     if (c && ((c != '\n' && c != ';' && c != '*') || redebug))
     {
       /* Either this line is not a comment, or we have to check
        * for outcommented debug statements.
        */
       line[0] = c; line[1] = '\0';
       fgets (line+1, 254, fin);
       if (c != '\n' && c != ';' && c != '*')
       {
         /* This line is not a comment, but may be an active
          * debug statement.
          */
         if (!redebug && !strncmp(line, "\tdebug\t", 7))
           putc(';', fout);
         fputs(line, fout);
       }
       else if (redebug && !strncmp(line, ";\tdebug\t", 8))
         /* This line is an outcommented debug statement
          * which has to be reactivated.
          */
         fputs(line+1, fout);
       else
       {
         /* This line is a comment: skip it */
         c = strlen(line);
         if (line[c-1] != '\n')
           while (c != '\n' && (c = getc(fin)) != EOF);
       }
     }
     else
       /* This line is a comment, and we don't care for
        * debug statements.
        */
       while (c != '\n' && (c = getc(fin)) != EOF);
  }
  fclose (fout);
  fclose (fin);
  return 0;
}
int main (int argc, char **argv) {
  if (argc < 3 || argc > 4) usage();
  if (argc >= 4 && !stricmp (*(argv+3), "redebug")) redebug = 1;
  if (!convert_file (*(argv+1), *(argv+2))) goto byebye;
byebye:
  exit(result);
}
