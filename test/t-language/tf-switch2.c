/* Switch test2
 *
 * Duff's Device should not work.
 * case/default should appear immediately within
 * a switch block and not in an inner block.
 * (As it is in Java.)
 */
 
void fun()
{
    int count = 10;
    int *src = allocate(10), *dest = allocate(10);
    int n, pos;

    n = (count + 3) / 4;
 
    switch(count % 4)
    {
        case 0:	do { dest[pos] = src[pos]; pos++;
        case 3:      dest[pos] = src[pos]; pos++;
        case 2:      dest[pos] = src[pos]; pos++;
        case 1:      dest[pos] = src[pos]; pos++;
                   } while(--n > 0);
    }
}
