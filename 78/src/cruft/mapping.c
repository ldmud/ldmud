/*-------------------------------------------------------------------------*/
#if 0  /* Not used anywhere */
void check_dirty(avoid)
    int avoid;
{
    struct mapping *m;
    mp_int i;

    m = dirty_mapping_head_hash.next_dirty;
    for (i = num_dirty_mappings - avoid; --i >= 0; m = m->hash->next_dirty) {
        struct hash_mapping *hm = m->hash;
        mp_int j;
        struct map_chain **mcp, *mc;

        j = hm->mask;
        mcp = hm->chains;
        do {
            mc = *mcp++;
            while (mc) {
                if ((p_int)mc & 0xff000003) /* The mask is machine dependent. */
                    fatal("check_dirty\n");
                mc = mc->next;
            }
        } while (--j >= 0);
    }
}
#endif

/*-------------------------------------------------------------------------*/
/* Only for debugging purposes */

void
compact_mappings (mp_int num)

{
    ...
        cm = m->condensed;
        hm = m->hash;
#ifdef DEBUG
        if (hm->ref) {
#if 1
            fatal("compact_mappings(): remaining ref count %ld!\n", hm->ref);
#else
            struct svalue *svp;
            int i;

            printf("compact_mappings(): remaining ref count %ld!\n", hm->ref);
#ifdef TRACE_CODE
            {
                last_instructions(TOTAL_TRACE_LENGTH, 1, 0);
            }
#endif
            printf("compact_mappings(): remaining ref count! %ld\n", hm->ref);
            if (hm->ref > 0) {
                for (mcp = hm->deleted; mcp; mcp = next) {
                    next = mcp->next;
                    svp = &mcp->key;
                    i = num_values;
                    do {
                        free_svalue(svp++);
                    } while (--i >= 0);
                    xfree( (char *)mcp );
                }
            }
#endif
        }
#endif /* DEBUG */
   ...

/***************************************************************************/

