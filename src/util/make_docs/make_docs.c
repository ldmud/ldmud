#include "../../machine.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifdef SYSDIR
#include <sys/dir.h>
#endif
#include <stdio.h>
#if defined(M_UNIX) || defined(solaris) || defined(linux)
#include <dirent.h>
#endif

#define EFUN "/efun"
#define LFUN "/lfun"

/*
 * This program will read all documentation for lfuns and efuns from the
 * mudlib/doc directory, and create a file of the format 'nroff -me',
 * to be included in the main documentation file, LPmud.doc.me.
 *
 * The files were not originally written to be formatted using nroff,
 * and may need some fixing.
 */
/*
#ifdef __STDC__
void load_files(char *path2, FILE *f);
void load_doc(char *name, FILE *f);
#endif
*/

#ifndef S_ISREG
#define S_ISREG(m)      (((m)&S_IFMT) == S_IFREG)
#endif

void load_doc(name, f)
    char *name;
    FILE *f;
{
    FILE *doc;
    int ch;
    struct stat st;

    doc = fopen(name, "r");
    if (doc == 0) {
	perror(name);
	return;
    }
    if (fstat(fileno(doc), &st) == -1 || !S_ISREG(st.st_mode)) {
	fclose(doc);
	return;
    }
    fprintf(f, ".SH    4 \"%s\"\n", name);
    while((ch = getc(doc)) != EOF)
	putc(ch, f);
    fclose(doc);
}

void load_files(path2, f)
    char *path2;
    FILE *f;
{
    DIR *dirp;
#if defined(_AIX) || defined(M_UNIX) || defined(solaris) || defined(linux)
    struct dirent *de;
#else
    struct direct *de;
#endif

    dirp = opendir(path2);
    if (dirp == 0) {
	fprintf(stderr, "No such directory '%s'\n", path2);
	exit(1);
    }
    if (chdir(path2) == -1) {
	perror(path2);
	exit(1);
    }
    for(de = readdir(dirp); de; de = readdir(dirp)) {
	if ((strcmp(de->d_name, ".") == 0 || strcmp(de->d_name, "..") == 0))
	    continue;
	if (strcmp(de->d_name, "README") == 0)
	    continue;
	load_doc(de->d_name, f);
    }
}

int main(argc, argv)
    char **argv;
    int argc;
{
    struct stat st;
    char *efun, *lfun, *docpath;
    FILE *f;

    if (argc != 2) {
	fprintf(stderr, "Usage: %s out_file\n", argv[0]);
	exit(1);
    }
    docpath = (char *)malloc(strlen(MUD_LIB) + 5);
    strcpy(docpath, MUD_LIB);
    strcat(docpath, "/doc");
    if (stat(docpath, &st) == -1) {
	perror(docpath);
	exit(1);
    }

    efun = (char *)malloc(strlen(docpath) + strlen(EFUN) + 1);
    strcpy(efun, docpath);
    strcat(efun, EFUN);

    lfun = (char *)malloc(strlen(docpath) + strlen(LFUN) + 1);
    strcpy(lfun, docpath);
    strcat(lfun, LFUN);

    f = fopen(argv[1], "w");
    if (f == NULL) {
	perror(argv[1]);
	exit(1);
    }
    fprintf(f, ".SH   3 \"efuns\"\n");
    load_files(efun, f);
    fprintf(f, ".SH   3 \"lfuns\"\n");
    load_files(lfun, f);
    fclose(f);
    exit(0);
}

