#include <string.h>

/*
 * The purpose of this file is to save memory space.
 * The most usual keywords are put in one big area, and
 * the LPC can have pointers into this area. The value nodes
 * should not point here, because there is some time overhead
 * to find a string.
 */

char string_space[] = {
    0,
    'm','o','v','e','_','p','l','a','y','e','r',0,
    'q','u','e','r','y','_','n','a','m','e',0,
    '#',0,
    'w','e','s','t',0,
    'e','a','s','t',0,
    'n','o','r','t','h',0,
    's','o','u','t','h',0,
    'a','t','t','a','c','k',0,
    'g','h','o','s','t',0,
    'I','t',' ','i','s',' ','d','a','r','k','.',0,
    'd','a','r','k',' ','r','o','o','m',0,
    'e','n','t','e','r',0,
    ' ','a','n','d',' ',0,
    'q','u','e','r','y','_','g','h','o','s','t',0,
    's','o','m','e',' ','m','i','s','t',0,
    'a','t','t','a','c','k','_','o','b','j','e','c','t',0,
    's','e','t','_','n','a','m','e',0,
    'h','i','t',0,
    ',',' ',0,
    'a','d','d','_','w','e','i','g','h','t',0,
    'h','i','t','_','p','l','a','y','e','r',0,
    'q','u','e','r','y','_','l','e','v','e','l',0,
    'T','h','e','r','e',' ','a','r','e',' ','t','w','o',' ','o','b','v','i','o','u','s',' ','e','x','i','t','s',',',' ',0,
    'T','o','o',' ','l','o','w',' ','o','n',' ','p','o','w','e','r','.',0,
    'a','t','t','a','c','k','e','d','_','b','y',0,
    's','e','t','_','s','h','o','r','t',0,
    's','e','t','_','m','o','n','e','y',0,
    'o','b','j','/','m','o','n','e','y',0,
    'c','a','n','_','p','u','t','_','a','n','d','_','g','e','t',0,
    's','e','t','_','t','y','p','e',0,
    'u','p',0,
    ' ','g','o','l','d',' ','c','o','i','n','s','.',0,
    'd','r','o','p',0,
    'p','l','a','y','e','r','s','/',0,
    's','e','t','_','a','l','i','a','s',0,
    's','e','t','_','l','e','v','e','l',0,
    'X',0,
    't','r','a','n','s','f','e','r','_','a','l','l','_','t','o',0,
    'w','e','a','r',0,
    'w','i','e','l','d',0,
    'd','o','o','r',0,
    's','t','o','p','_','w','e','a','r','i','n','g',0,
    'a','r','m','o','u','r','_','c','l','a','s','s',0,
    'c','l','o','s','e',0,
    'o','p','e','n',0,
    's','e','t','_','w','c',0,
    'c','h','u','r','c','h',0,
    'r','o','o','m','/','e','l','e','v','a','t','o','r',0,
    's','e','t','_','m','a','t','c','h',0,
    '%','s',' ','%','s',0,
    'a','d','d','_','e','x','p',0,
    'a','d','d','_','m','o','n','e','y',0,
    'c','a','s','t','l','e',0,
    'l','e','a','v','e','s',0,
    'l','o','a','d','_','c','h','a','t',0,
    'r','e','a','d',0,
    's','e','t','_','l','o','n','g',0,
    's','e','t','_','v','a','l','u','e',0,
    ' ',' ',' ',' ','T','h','e',' ','o','n','l','y',' ','o','b','v','i','o','u','s',' ','e','x','i','t',' ','i','s',' ','t','o',' ',0,
    'A',' ','l','a','r','g','e',' ','o','p','e','n',' ','p','l','a','i','n',0,
    'a','d','d','_','a','l','i','g','n','m','e','n','t',0,
    's','e','t','_','a','c',0,
    's','e','t','_','a','l',0,
    'w','e','a','p','o','n','_','c','l','a','s','s',0,
    'a','r','r','i','v','e','s',0,
    'c','l','i','m','b',0,
    'o','b','j','/','m','o','n','s','t','e','r',0,
    's','e','t','_','a','g','g','r','e','s','s','i','v','e',0,
    's','e','t','_','w','e','i','g','h','t',0,
    'q','u','e','r','y','_','m','o','n','e','y',0,
    's','e','c','o','n','d','_','l','i','f','e',0,
    ' ', 0,
    'l','o','n','g',0,
    's','h','o','r','t',0,
    'r','e','s','e','t',0,
    'i','n','i','t',0,
    'e','x','i','t',0,
    'm','o','v','e',0,
    'm','o','v','e','1',0,
    'm','o','v','e','2',0,
    'm','o','v','e','3',0,
    'm','o','v','e','4',0,
    'O','k','.','\n',0,
    '\n',0,
    '.','\n',0,
    'g','e','t',0,
    'q','u','e','r','y','_','v','a','l','u','e',0,
    'q','u','e','r','y','_','w','e','i','g','h','t',0,
    'i','d',0,
    'h','e','a','r','t','_','b','e','a','t',0,
    0
};

char *end_of_string_space;

void init_string_space() {
    char *p;

    for (p = string_space;; p++) {
	if (*p)
	    continue;
	if (p[1] != 0)
	    continue;
	break;
    }
    end_of_string_space = p+2;
}

int tot_string_space;

char *find_string_space(str)
    char *str;
{
    register char *p;
    register int len = strlen(str);

    for (p = string_space+1; p < end_of_string_space; p++) {
	if (p[len] != '\0')
	    continue;
	if (strcmp(p, str) == 0) {
	    tot_string_space += strlen(str);
	    return p;
	}
	p += len - 1;
    }
    return 0;
}
