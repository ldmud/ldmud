#ifndef LPC_COMPILE_STRING_H_
#define LPC_COMPILE_STRING_H_

/* Definition of flags for compile_string()
 */

#define CS_COMPILE_EXPRESSION             1  /* Compile as expression.    */
#define CS_COMPILE_BLOCK                  2  /* Compile as block.         */
#define CS_ASYNC                          4  /* Compile as coroutine.     */
#define CS_USE_OBJECT_VARIABLES           8  /* Use object's variables.   */
#define CS_USE_OBJECT_FUNCTIONS          16  /* Use object's functions.   */
#define CS_USE_OBJECT_STRUCTS            32  /* Use object's struct defs. */

#endif /* LPC_COMPILE_STRING_H_ */
