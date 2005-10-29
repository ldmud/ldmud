#ifndef LPC_INPUT_TO_H_
#define LPC_INPUT_TO_H_

/* Mode values recognized by the efun input_to() */

#define INPUT_NOECHO         1  /* Don't echo the next line typed */
#define INPUT_CHARMODE       2  /* Switch into/out of charmode */
#define INPUT_PROMPT         4  /* Use a custom prompt */
#define INPUT_NO_TELNET      8  /* Switch into/out of charmode on the driver
                                 * side only.
                                 */
#define INPUT_APPEND        16  /* Append the input_to to the list of already
                                 * pending input_to's.
                                 */
#define INPUT_IGNORE_BANG  128  /* Disallow the '!' escape */

#endif /* LPC_INPUT_TO_H_ */
