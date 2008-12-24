/* Switch test1 (Bug #584)
 *
 * The declaration of a variable within a switch block
 * should not span over multiple case/default statements.
 * (As it is in C++.)
 */
 
int fun()
{
    switch(1)
    {
	case 0:
	    int i=1;
	    return i;
	
	case 1:
	    i=2;
	    return i;
    }
    
    return -1;
}
