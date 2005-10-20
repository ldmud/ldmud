/*-------------------------------------------------------------------------*/
Recursive assignment is useful sometimes.

#if 0
/*
 * Check that an assignment to an array item is not cyclic.
 */
static void check_for_recursion(vec, v)
    struct vector *vec, *v;
{
    register int i;

    if (vec->user)
	vec->user->cost++;
    eval_cost++;
    if (v == vec)
	error("Recursive asignment of vectors.\n");
    for (i=0; i<v->size; i++) {
	if (v->item[i].type == T_POINTER)
	    check_for_recursion(vec, v->item[i].u.vec);
    }
}
#endif


/*-------------------------------------------------------------------------*/
#if 0 /* Not used anywhere... */
/* Turns a structured array of elements into a flat array of elements.
   */
static int num_elems(arr)
    struct vector *arr;
{
    int cnt,il;

    cnt = arr->size;

    for (il=0;il<arr->size;il++)
	if (arr->item[il].type == T_POINTER)
	    cnt += num_elems(arr->item[il].u.vec) - 1;
    return cnt;
}

struct vector *flatten_array(arr)
    struct vector *arr;
{
    int max, cnt, il, il2;
    struct vector *res, *dres;

    if (arr->size < 1)
	return allocate_array(0);

    max = num_elems(arr);

    if (arr->size == max)
	return arr;

    if (max == 0) 	    /* In the case arr is an array of empty arrays */
	return allocate_array(0);

    res = allocate_array(max);

    for (cnt = 0 , il = 0; il < arr->size; il++)
	if (arr->item[il].type != T_POINTER)
	    assign_svalue(&res->item[cnt++],&arr->item[il]);
	else {
	    dres = flatten_array(arr->item[il].u.vec);
	    for (il2=0;il2<dres->size;il2++)
		assign_svalue(&res->item[cnt++],&dres->item[il2]);
	    free_vector(dres);
	}

    return res;
}
#endif

/*-------------------------------------------------------------------------*/
#if 0 /* Not used anywhere... */
/* sum_array, processes each element in the array together with the
              results from the previous element. Starting value is 0.
	      Note! This routine allocates a struct svalue which it returns.
	      This value should be pushed to the stack and then freed.
   */
struct svalue *sum_array (arr, func, ob, extra)
    struct vector *arr;
    char *func;
    struct object *ob;
    struct svalue *extra;
{
    struct svalue *ret, v;

    int cnt;

    v.type = T_NUMBER;
    v.u.number = 0;

    for (cnt=0;cnt<arr->size;cnt++) {
	push_svalue(&arr->item[cnt]);
	push_svalue(&v);
	if (extra) {
	    push_svalue (extra);
	    ret = apply(func, ob, 3);
	} else {
	    ret = apply(func,ob,2);
	}
	if (ret)
	    v = *ret;
    }

    ret = (struct svalue *) xalloc(sizeof(struct svalue));
    *ret = v;

    return ret;
}
#endif


/*-------------------------------------------------------------------------*/
Old version

#if 0
struct vector *order_alist(inlist)
    struct vector *inlist;
{
    struct vector *outlist;
    struct svalue *inlists, *outlists, *root, *inpnt, *insval;
    int listnum, keynum, i, j;

    listnum = inlist->size;
    inlists = inlist->item;
    keynum = inlists[0].u.vec->size;
    root = inlists[0].u.vec->item;
    /* transform inlists[i].u.vec->item[j] into a heap, starting at the top */
    insval = (struct svalue*)xalloc(sizeof(struct svalue[1])*listnum);
    for(j=0,inpnt=root; j<keynum; j++,inpnt++) {
	int curix, parix;

	/* make sure that strings can be compared by their pointer */
	if (inpnt->type == T_STRING && inpnt->x.string_type != STRING_SHARED) {
	    char *str = make_shared_string(inpnt->u.string);
	    free_svalue(inpnt);
	    inpnt->type = T_STRING;
	    inpnt->x.string_type = STRING_SHARED;
	    inpnt->u.string = str;
	}
	/* propagate the new element up in the heap as much as necessary */
	for (i=0; i<listnum; i++) {
	    insval[i] = inlists[i].u.vec->item[j];
	    /* but first ensure that it contains no destructed object */
	    if (insval[i].type == T_OBJECT
	      && insval[i].u.ob->flags & O_DESTRUCTED) {
		free_object(insval[i].u.ob, "order_alist");
	        inlists[i].u.vec->item[j] = insval[i] = const0;
	    }
	}
	for(curix = j; curix; curix=parix) {
	    parix = (curix-1)>>1;
	    if ( alist_cmp(&root[parix], &root[curix]) > 0 ) {
		for (i=0; i<listnum; i++) {
		    inlists[i].u.vec->item[curix] =
		      inlists[i].u.vec->item[parix];
		    inlists[i].u.vec->item[parix] = insval[i];
		}
	    }
	}
    }
    xfree((char*)insval);
    outlist = allocate_array(listnum);
    outlists = outlist->item;
    for (i=0; i<listnum; i++) {
	outlists[i].type  = T_POINTER;
	outlists[i].u.vec = allocate_array(keynum);
    }
    for(j=0; j<keynum; j++) {
	int curix;

	for (i=0;  i<listnum; i++) {
	    outlists[i].u.vec->item[j] = inlists[i].u.vec->item[0];
	}
	for (curix=0; ; ) {
	    int child, child2;

	    child = curix+curix+1;
	    child2 = child+1;
	    if (child2<keynum && root[child2].type != T_INVALID
	      && (root[child].type == T_INVALID ||
		alist_cmp(&root[child], &root[child2]) > 0))
	    {
		child = child2;
	    }
	    if (child<keynum && root[child].type != T_INVALID) {
		for (i=0; i<listnum; i++) {
		    inlists[i].u.vec->item[curix] =
		      inlists[i].u.vec->item[child];
		}
		curix = child;
	    } else break;
	}
	for (i=0; i<listnum; i++) {
	    inlists[i].u.vec->item[curix].type = T_INVALID;
	}
    }
    return outlist;
}
#endif

/*-------------------------------------------------------------------------*/
#if 0 /* Not used anywhere */

struct vector *symmetric_difference_alist(a1, a2)
    struct vector *a1,*a2;
{
    struct vector *a3;
    int d, l, i1, i2, a1s, a2s;

    a1s = VEC_SIZE(a1);
    a2s = VEC_SIZE(a2);
    a3 = allocate_array( a1s + a2s );
    for (i1=i2=l=0; i1 < a1s && i2 < a2s; ) {
	d = alist_cmp(&a1->item[i1], &a2->item[i2]);
	if (d<0)
	    assign_svalue_no_free(&a3->item[l++], &a1->item[i1++]);
	else if (d>0)
	    assign_svalue_no_free(&a3->item[l++], &a2->item[i2++]);
	else {
	    i1++;
	    i2++;
	}
    }
    while (i1 < a1s )
	    assign_svalue_no_free(&a3->item[l++], &a1->item[i1++]);
    while (i2 < a2s )
	    assign_svalue_no_free(&a3->item[l++], &a2->item[i2++]);
    return shrink_array(a3, l);
}
#endif

/***************************************************************************/

