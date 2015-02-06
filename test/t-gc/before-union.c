/* The driver puts all union types with a common head in a
 * singly-linked list. The GC rebuilds that list, we try to
 * trick it into entering the following type twice, because
 * <float|int> is a static union object with little guard
 * against being processed twice.
 */
float|int a;
float|int b;

float|string c;
string|int d;
