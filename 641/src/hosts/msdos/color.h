/* Colour */


#undef BLINK  /* override gppconio.h */
#define BLINK   0x8000
#define FG_I    0x0800    /* intensity  */

/* background */

#define BG_R   0x4000    /* red   */
#define BG_G   0x2000    /* green */
#define BG_B   0x1000    /* blue  */


/* foreground */

#define FG_R   0x0400    /* red   */
#define FG_G   0x0200    /* green */
#define FG_B   0x0100    /* blue  */


/* mono */

#define BG      0x7000    /* turn on background */
#define FG      0x0700    /* turn on foreground */
#define FG_LINE 0x0100    /* underline          */

