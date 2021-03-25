/* This sefun is to provide a replacement for the efun debug_info().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(debug_info)

#include <driver_info.h>
#include <debug_info.h>
#include <object_info.h>

mixed debug_info(int what, varargs mixed* args)
{
    if (sizeof(args) > 2)
        raise_error("Too many arguments to debug_info\n");

    switch(what)
    {
        default:
            raise_error(sprintf("Illegal value %d for debug_info().\n", what));

        case DINFO_OBJECT:
        {
            object ob;

            if (sizeof(args) != 1)
                raise_error("bad number of arguments to debug_info\n");
            if (!objectp(args[0]))
                raise_error("bag arg 2 to debug_info().\n");

            ob = args[0];
            printf("O_HEART_BEAT      : %s\n", efun::object_info(ob, OC_HEART_BEAT)       ? "TRUE" : "FALSE");
            printf("O_ENABLE_COMMANDS : %s\n", efun::object_info(ob, OC_COMMANDS_ENABLED) ? "TRUE" : "FALSE");
            printf("O_CLONE           : %s\n", efun::clonep(ob)                           ? "TRUE" : "FALSE");
            printf("O_DESTRUCTED      : FALSE\n");
            printf("O_SWAPPED         : %s\n", efun::object_info(ob, OI_SWAPPED)          ? "TRUE" : "FALSE");
            printf("O_ONCE_INTERACTIVE: %s\n", efun::object_info(ob, OI_ONCE_INTERACTIVE) ? "TRUE" : "FALSE");
            printf("O_RESET_STATE     : %s\n", efun::object_info(ob, OI_RESET_STATE)      ? "TRUE" : "FALSE");
            printf("O_WILL_CLEAN_UP   : %s\n", efun::object_info(ob, OI_WILL_CLEAN_UP)    ? "TRUE" : "FALSE");
            printf("O_REPLACED        : %s\n", efun::object_info(ob, OI_REPLACED)         ? "TRUE" : "FALSE");
            printf("time_reset  : %d\n",       efun::object_info(ob, OI_NEXT_RESET_TIME));
            printf("time_of_ref : %d\n",       efun::object_info(ob, OI_LAST_REF_TIME));
            printf("ref         : %d\n",       efun::object_info(ob, OI_OBJECT_REFS));

            int gticks = efun::object_info(ob, OI_GIGATICKS);
            if(gticks)
                printf("evalcost   :  %d%09d\n", gticks, efun::object_info(ob, OI_TICKS));
            else
                printf("evalcost   :  %d\n",   efun::object_info(ob, OI_TICKS));

            printf("swap_num    : %d\n",       efun::object_info(ob, OI_SWAP_NUM));
            printf("name        : '%s'\n",     efun::object_name(ob));
            printf("load_name   : '%s'\n",     efun::load_name(ob));

            object next_ob = efun::object_info(ob, OI_OBJECT_NEXT);
            if (next_ob)
                printf("next_all    : OBJ(%s)\n", efun::object_name(next_ob));

            object prev_ob = efun::object_info(ob, OI_OBJECT_PREV);
            if (prev_ob)
                printf("Previous object in object list: OBJ(%s)\n", efun::object_name(prev_ob));
            else
                printf("This object is the head of the object list.\n");
            break;
        }

        case DINFO_MEMORY:
        {
            object ob;

            if (sizeof(args) != 1)
                raise_error("bad number of arguments to debug_info\n");
            if (!objectp(args[0]))
                raise_error("bag arg 2 to debug_info().\n");

            ob = args[0];

            printf("program ref's %3d\n",   efun::object_info(ob, OI_PROG_REFS));
            printf("Name: '%s'\n",          efun::program_name(ob));
            printf("program size    %6d\n", efun::object_info(ob, OI_PROG_SIZE));
            printf("num func's:  %3d (%4d)\n",
                                            efun::object_info(ob, OI_NUM_FUNCTIONS),
                                            efun::object_info(ob, OI_SIZE_FUNCTIONS));
            printf("num vars:    %3d (%4d)\n",
                                            efun::object_info(ob, OI_NUM_VARIABLES),
                                            efun::object_info(ob, OI_SIZE_VARIABLES));

            printf("num strings: %3d (%4d) : overhead %d + data %d (%d)\n",
                                            efun::object_info(ob, OI_NUM_STRINGS),
                                            efun::object_info(ob, OI_SIZE_STRINGS) + efun::object_info(ob, OI_SIZE_STRINGS_DATA),
                                            efun::object_info(ob, OI_SIZE_STRINGS),
                                            efun::object_info(ob, OI_SIZE_STRINGS_DATA),
                                            efun::object_info(ob, OI_SIZE_STRINGS_DATA_TOTAL));

            printf("num inherits %3d (%4d)\n",
                                            efun::object_info(ob, OI_NUM_INHERITED),
                                            efun::object_info(ob, OI_SIZE_INHERITED));

            printf("total size      %6d\n", efun::object_info(ob, OI_PROG_SIZE_TOTAL));

            printf("data size       %6d (%6d\n",
                                            efun::object_info(ob, OI_DATA_SIZE),
                                            efun::object_info(ob, OI_DATA_SIZE_TOTAL));
            break;
        }

        case DINFO_OBJLIST:
        {
            if (sizeof(args) == 0)
                return efun::objects(0, 1)[0];

            if (sizeof(args) == 1)
            {
                object * obs = efun::objects(args[0], 1);
                return sizeof(obs) && obs[0];
            }
            else
                return efun::objects(args[0], args[1]);
            break;
        }

        case DINFO_MALLOC:
            write(debug_info(DINFO_STATUS, "malloc"));
            break;

        case DINFO_STATUS:
        {
            string which;
            int opt;

            if (sizeof(args) > 1)
                raise_error("bad number of arguments to debug_info\n");

            if (!sizeof(args) || !args[0])
                which = "";
            else if(stringp(args[0]))
                which = args[0];
            else
                raise_error("bag arg 2 to debug_info().\n");

            switch(which)
            {
                case "":
                    opt = DI_STATUS_TEXT_MEMORY;
                    break;

                case "tables":
                    opt = DI_STATUS_TEXT_TABLES;
                    break;

                case "swap":
                    opt = DI_STATUS_TEXT_SWAP;
                    break;

                case "malloc":
                    opt = DI_STATUS_TEXT_MALLOC;
                    break;

                case "malloc extstats":
                    opt = DI_STATUS_TEXT_MALLOC_EXTENDED;
                    break;

                default:
                    return 0;
            }

            return efun::driver_info(opt);
        }

        case DINFO_DUMP:
        {
            int opt;

            if (!sizeof(args))
                raise_error("bad number of arguments to debug_info\n");

            if(!stringp(args[0]))
                raise_error("bag arg 2 to debug_info().\n");

            switch(args[0])
            {
                case "objects":
                    opt = DDI_OBJECTS;
                    break;

                case "destructed":
                    opt = DDI_OBJECTS_DESTRUCTED;
                    break;

                case "opcodes":
                    opt = DDI_OPCODES;
                    break;

                case "memory":
                    opt = DDI_MEMORY;
                    break;

                default:
                    raise_error(sprintf("Bad argument '%s' to debug_info(DINFO_DUMP).\n", args[0]));
                    return 0;
            }

            return efun::dump_driver_info(opt, args[1..1]...);
        }

        case DINFO_DATA:
        {
            mixed * result;

            if (!sizeof(args))
                raise_error("bad number of arguments to debug_info\n");

            if (!intp(args[0]))
                raise_error("bag arg 2 to debug_info().\n");

            if (sizeof(args) == 2 && !intp(args[1]))
                raise_error("bag arg 3 to debug_info().\n");

            switch(args[0])
            {
                case DID_STATUS:
                    result = allocate(DID_STATUS_MAX);

                    result[DID_ST_ACTIONS]               = efun::driver_info(DI_NUM_ACTIONS);
                    result[DID_ST_ACTIONS_SIZE]          = efun::driver_info(DI_SIZE_ACTIONS);
                    result[DID_ST_SHADOWS]               = efun::driver_info(DI_NUM_SHADOWS);
                    result[DID_ST_SHADOWS_SIZE]          = efun::driver_info(DI_SIZE_SHADOWS);

                    result[DID_ST_OBJECTS]               = efun::driver_info(DI_NUM_OBJECTS);
                    result[DID_ST_OBJECTS_SIZE]          = efun::driver_info(DI_SIZE_OBJECTS);
                    result[DID_ST_OBJECTS_SWAPPED]       = efun::driver_info(DI_NUM_OBJECTS_SWAPPED);
                    result[DID_ST_OBJECTS_SWAP_SIZE]     = efun::driver_info(DI_SIZE_OBJECTS_SWAPPED);
                    result[DID_ST_OBJECTS_LIST]          = efun::driver_info(DI_NUM_OBJECTS_IN_LIST);
                    result[DID_ST_OBJECTS_NEWLY_DEST]    = efun::driver_info(DI_NUM_OBJECTS_NEWLY_DESTRUCTED);
                    result[DID_ST_OBJECTS_DESTRUCTED]    = efun::driver_info(DI_NUM_OBJECTS_DESTRUCTED);
                    result[DID_ST_OBJECTS_PROCESSED]     = efun::driver_info(DI_NUM_OBJECTS_LAST_PROCESSED);
                    result[DID_ST_OBJECTS_AVG_PROC]      = efun::driver_info(DI_LOAD_AVERAGE_PROCESSED_OBJECTS_RELATIVE);

                    result[DID_ST_OTABLE]                = efun::driver_info(DI_NUM_OBJECTS_IN_TABLE);
                    result[DID_ST_OTABLE_SLOTS]          = efun::driver_info(DI_NUM_OBJECT_TABLE_SLOTS);
                    result[DID_ST_OTABLE_SIZE]           = efun::driver_info(DI_SIZE_OBJECT_TABLE);

                    result[DID_ST_HBEAT_OBJS]            = efun::driver_info(DI_NUM_HEARTBEATS);
                    result[DID_ST_HBEAT_CALLS]           = efun::driver_info(DI_NUM_HEARTBEAT_ACTIVE_CYCLES);
                    result[DID_ST_HBEAT_CALLS_TOTAL]     = efun::driver_info(DI_NUM_HEARTBEAT_TOTAL_CYCLES);
                    result[DID_ST_HBEAT_SLOTS]           = efun::driver_info(DI_NUM_HEARTBEATS);
                    result[DID_ST_HBEAT_SIZE]            = efun::driver_info(DI_SIZE_HEARTBEATS);
                    result[DID_ST_HBEAT_PROCESSED]       = efun::driver_info(DI_NUM_HEARTBEATS_LAST_PROCESSED);
                    result[DID_ST_HBEAT_AVG_PROC]        = efun::driver_info(DI_LOAD_AVERAGE_PROCESSED_HEARTBEATS_RELATIVE);

                    result[DID_ST_CALLOUTS]              = efun::driver_info(DI_NUM_CALLOUTS);
                    result[DID_ST_CALLOUT_SIZE]          = efun::driver_info(DI_SIZE_CALLOUTS);

                    result[DID_ST_ARRAYS]                = efun::driver_info(DI_NUM_ARRAYS);
                    result[DID_ST_ARRAYS_SIZE]           = efun::driver_info(DI_SIZE_ARRAYS);

                    result[DID_ST_MAPPINGS]              = efun::driver_info(DI_NUM_MAPPINGS);
                    result[DID_ST_MAPPINGS_SIZE]         = efun::driver_info(DI_SIZE_MAPPINGS);
                    result[DID_ST_HYBRID_MAPPINGS]       = efun::driver_info(DI_NUM_MAPPINGS_HYBRID);
                    result[DID_ST_HASH_MAPPINGS]         = efun::driver_info(DI_NUM_MAPPINGS_HASH);

                    result[DID_ST_STRUCTS]               = efun::driver_info(DI_NUM_STRUCTS);
                    result[DID_ST_STRUCTS_SIZE]          = efun::driver_info(DI_SIZE_STRUCTS);
                    result[DID_ST_STRUCT_TYPES]          = efun::driver_info(DI_NUM_STRUCT_TYPES);
                    result[DID_ST_STRUCT_TYPES_SIZE]     = efun::driver_info(DI_SIZE_STRUCT_TYPES);

                    result[DID_ST_PROGS]                 = efun::driver_info(DI_NUM_PROGS);
                    result[DID_ST_PROGS_SIZE]            = efun::driver_info(DI_SIZE_PROGS);

                    result[DID_ST_PROGS_SWAPPED]         = efun::driver_info(DI_NUM_PROGS_SWAPPED);
                    result[DID_ST_PROGS_SWAP_SIZE]       = efun::driver_info(DI_SIZE_PROGS_SWAPPED);

                    result[DID_ST_USER_RESERVE]          = efun::driver_info(DI_MEMORY_RESERVE_USER);
                    result[DID_ST_MASTER_RESERVE]        = efun::driver_info(DI_MEMORY_RESERVE_MASTER);
                    result[DID_ST_SYSTEM_RESERVE]        = efun::driver_info(DI_MEMORY_RESERVE_SYSTEM);

                    result[DID_ST_ADD_MESSAGE]           = efun::driver_info(DI_NUM_MESSAGES_OUT);
                    result[DID_ST_PACKETS]               = efun::driver_info(DI_NUM_PACKETS_OUT);
                    result[DID_ST_PACKET_SIZE]           = efun::driver_info(DI_SIZE_PACKETS_OUT);
                    result[DID_ST_PACKETS_IN]            = efun::driver_info(DI_NUM_PACKETS_IN);
                    result[DID_ST_PACKET_SIZE_IN]        = efun::driver_info(DI_SIZE_PACKETS_IN);

                    result[DID_ST_APPLY]                 = efun::driver_info(DI_NUM_FUNCTION_NAME_CALLS);
                    result[DID_ST_APPLY_HITS]            = efun::driver_info(DI_NUM_FUNCTION_NAME_CALL_HITS);

                    result[DID_ST_STRINGS]               = efun::driver_info(DI_NUM_VIRTUAL_STRINGS);
                    result[DID_ST_STRING_SIZE]           = efun::driver_info(DI_SIZE_STRINGS);
                    result[DID_ST_STR_TABLE_SIZE]        = efun::driver_info(DI_SIZE_STRING_TABLE);
                    result[DID_ST_STR_OVERHEAD]          = efun::driver_info(DI_SIZE_STRING_OVERHEAD);
                    result[DID_ST_UNTABLED]              = efun::driver_info(DI_NUM_STRINGS_UNTABLED);
                    result[DID_ST_UNTABLED_SIZE]         = efun::driver_info(DI_SIZE_STRINGS_UNTABLED);
                    result[DID_ST_TABLED]                = efun::driver_info(DI_NUM_STRINGS_TABLED);
                    result[DID_ST_TABLED_SIZE]           = efun::driver_info(DI_SIZE_STRINGS_TABLED);
                    result[DID_ST_STR_SEARCHES]          = efun::driver_info(DI_NUM_STRING_TABLE_LOOKUPS_BY_INDEX);
                    result[DID_ST_STR_SEARCHLEN]         = efun::driver_info(DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_INDEX);
                    result[DID_ST_STR_SEARCHES_BYVALUE]  = efun::driver_info(DI_NUM_STRING_TABLE_LOOKUPS_BY_VALUE);
                    result[DID_ST_STR_SEARCHLEN_BYVALUE] = efun::driver_info(DI_NUM_STRING_TABLE_LOOKUP_STEPS_BY_VALUE);
                    result[DID_ST_STR_CHAINS]            = efun::driver_info(DI_NUM_STRING_TABLE_SLOTS_USED);
                    result[DID_ST_STR_ADDED]             = efun::driver_info(DI_NUM_STRING_TABLE_STRINGS_ADDED);
                    result[DID_ST_STR_DELETED]           = efun::driver_info(DI_NUM_STRING_TABLE_STRINGS_REMOVED);
                    result[DID_ST_STR_COLLISIONS]        = efun::driver_info(DI_NUM_STRING_TABLE_COLLISIONS);
                    result[DID_ST_STR_FOUND]             = efun::driver_info(DI_NUM_STRING_TABLE_HITS_BY_INDEX);
                    result[DID_ST_STR_FOUND_BYVALUE]     = efun::driver_info(DI_NUM_STRING_TABLE_HITS_BY_VALUE);

                    result[DID_ST_RX_CACHED]             = efun::driver_info(DI_NUM_REGEX);
                    result[DID_ST_RX_TABLE]              = efun::driver_info(DI_NUM_REGEX_TABLE_SLOTS);
                    result[DID_ST_RX_TABLE_SIZE]         = efun::driver_info(DI_SIZE_REGEX);
                    result[DID_ST_RX_REQUESTS]           = efun::driver_info(DI_NUM_REGEX_LOOKUPS);
                    result[DID_ST_RX_REQ_FOUND]          = efun::driver_info(DI_NUM_REGEX_LOOKUP_HITS);
                    result[DID_ST_RX_REQ_COLL]           = efun::driver_info(DI_NUM_REGEX_LOOKUP_COLLISIONS);

                    result[DID_ST_MB_FILE]               = efun::driver_info(DI_SIZE_BUFFER_FILE);
                    result[DID_ST_MB_SWAP]               = efun::driver_info(DI_SIZE_BUFFER_SWAP);

                    result[DID_ST_BOOT_TIME]             = efun::driver_info(DI_BOOT_TIME);
                    break;

                case DID_SWAP:
                    result = allocate(DID_SWAP_MAX);

                    result[DID_SW_PROGS]                 = efun::driver_info(DI_NUM_PROGS_SWAPPED);
                    result[DID_SW_PROG_SIZE]             = efun::driver_info(DI_SIZE_PROGS_SWAPPED);
                    result[DID_SW_PROG_UNSWAPPED]        = efun::driver_info(DI_NUM_PROGS_UNSWAPPED);
                    result[DID_SW_PROG_U_SIZE]           = efun::driver_info(DI_SIZE_PROGS_UNSWAPPED);
                    result[DID_SW_VARS]                  = efun::driver_info(DI_NUM_OBJECTS_SWAPPED);
                    result[DID_SW_VAR_SIZE]              = efun::driver_info(DI_SIZE_OBJECTS_SWAPPED);
                    result[DID_SW_FREE]                  = efun::driver_info(DI_NUM_SWAP_BLOCKS_FREE);
                    result[DID_SW_FREE_SIZE]             = efun::driver_info(DI_SIZE_SWAP_BLOCKS_FREE);
                    result[DID_SW_FILE_SIZE]             = efun::driver_info(DI_SIZE_SWAP_BLOCKS);
                    result[DID_SW_REUSED]                = efun::driver_info(DI_SIZE_SWAP_BLOCKS_REUSED);
                    result[DID_SW_SEARCHES]              = efun::driver_info(DI_NUM_SWAP_BLOCKS_REUSE_LOOKUPS);
                    result[DID_SW_SEARCH_LEN]            = efun::driver_info(DI_NUM_SWAP_BLOCKS_REUSE_LOOKUP_STEPS);
                    result[DID_SW_F_SEARCHES]            = efun::driver_info(DI_NUM_SWAP_BLOCKS_FREE_LOOKUPS);
                    result[DID_SW_F_SEARCH_LEN]          = efun::driver_info(DI_NUM_SWAP_BLOCKS_FREE_LOOKUP_STEPS);
                    result[DID_SW_COMPACT]               = efun::driver_info(DC_SWAP_COMPACT_MODE);
                    result[DID_SW_RECYCLE_FREE]          = efun::driver_info(DI_SWAP_RECYCLE_PHASE);
                    break;

                case DID_MEMORY:
                    result = allocate(DID_MEMORY_MAX);

                    result[DID_MEM_NAME]                 = efun::driver_info(DI_MEMORY_ALLOCATOR_NAME);
                    result[DID_MEM_SBRK]                 = efun::driver_info(DI_NUM_SYS_ALLOCATED_BLOCKS);
                    result[DID_MEM_SBRK_SIZE]            = efun::driver_info(DI_SIZE_SYS_ALLOCATED_BLOCKS);
                    result[DID_MEM_LARGE]                = efun::driver_info(DI_NUM_LARGE_BLOCKS_ALLOCATED);
                    result[DID_MEM_LARGE_SIZE]           = efun::driver_info(DI_SIZE_LARGE_BLOCKS_ALLOCATED);
                    result[DID_MEM_LFREE]                = efun::driver_info(DI_NUM_LARGE_BLOCKS_FREE);
                    result[DID_MEM_LFREE_SIZE]           = efun::driver_info(DI_SIZE_LARGE_BLOCKS_FREE);
                    result[DID_MEM_LWASTED]              = efun::driver_info(DI_NUM_LARGE_BLOCKS_WASTE);
                    result[DID_MEM_LWASTED_SIZE]         = efun::driver_info(DI_SIZE_LARGE_BLOCKS_WASTE);
                    result[DID_MEM_CHUNK]                = efun::driver_info(DI_NUM_SMALL_BLOCK_CHUNKS);
                    result[DID_MEM_CHUNK_SIZE]           = efun::driver_info(DI_SIZE_SMALL_BLOCK_CHUNKS);
                    result[DID_MEM_SMALL]                = efun::driver_info(DI_NUM_SMALL_BLOCKS_ALLOCATED);
                    result[DID_MEM_SMALL_SIZE]           = efun::driver_info(DI_SIZE_SMALL_BLOCKS_ALLOCATED);
                    result[DID_MEM_SFREE]                = efun::driver_info(DI_NUM_SMALL_BLOCKS_FREE);
                    result[DID_MEM_SFREE_SIZE]           = efun::driver_info(DI_SIZE_SMALL_BLOCKS_FREE);
                    result[DID_MEM_SWASTED]              = efun::driver_info(DI_NUM_SMALL_BLOCKS_WASTE);
                    result[DID_MEM_SWASTED_SIZE]         = efun::driver_info(DI_SIZE_SMALL_BLOCKS_WASTE);
                    result[DID_MEM_MINC_CALLS]           = efun::driver_info(DI_NUM_INCREMENT_SIZE_CALLS);
                    result[DID_MEM_MINC_SUCCESS]         = efun::driver_info(DI_NUM_INCREMENT_SIZE_CALL_SUCCESSES);
                    result[DID_MEM_MINC_SIZE]            = efun::driver_info(DI_SIZE_INCREMENT_SIZE_CALL_DIFFS);
                    result[DID_MEM_PERM]                 = efun::driver_info(DI_NUM_UNMANAGED_BLOCKS);
                    result[DID_MEM_PERM_SIZE]            = efun::driver_info(DI_SIZE_UNMANAGED_BLOCKS);
                    result[DID_MEM_CLIB]                 = efun::driver_info(DI_NUM_REPLACEMENT_MALLOC_CALLS);
                    result[DID_MEM_CLIB_SIZE]            = efun::driver_info(DI_SIZE_REPLACEMENT_MALLOC_CALLS);
                    result[DID_MEM_OVERHEAD]             = efun::driver_info(DI_SIZE_SMALL_BLOCK_OVERHEAD);
                    result[DID_MEM_ALLOCATED]            = efun::driver_info(DI_SIZE_MEMORY_USED) + efun::driver_info(DI_SIZE_MEMORY_OVERHEAD);
                    result[DID_MEM_USED]                 = efun::driver_info(DI_SIZE_MEMORY_USED);
                    result[DID_MEM_TOTAL_UNUSED]         = efun::driver_info(DI_SIZE_MEMORY_UNUSED);
                    result[DID_MEM_DEFRAG_CALLS]         = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_FULL) + efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_TARGETED);
                    result[DID_MEM_DEFRAG_CALLS_REQ]     = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_CALLS_TARGETED);
                    result[DID_MEM_DEFRAG_REQ_SUCCESS]   = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_CALL_TARGET_HITS);
                    result[DID_MEM_DEFRAG_BLOCKS_INSPECTED] = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_INSPECTED);
                    result[DID_MEM_DEFRAG_BLOCKS_MERGED] = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_MERGED);
                    result[DID_MEM_DEFRAG_BLOCKS_RESULT] = efun::driver_info(DI_NUM_MEMORY_DEFRAGMENTATION_BLOCKS_RESULTING);
                    result[DID_MEM_AVL_NODES]            = efun::driver_info(DI_NUM_FREE_BLOCKS_AVL_NODES);
                    result[DID_MEM_EXT_STATISTICS]       = efun::driver_info(DI_MEMORY_EXTENDED_STATISTICS);
                    break;
            }

            if (sizeof(args) == 2)
            {
                int idx = args[0];
                if (idx < 0 || idx >= sizeof(result))
                    raise_error(sprintf("Illegal index for debug_info(): %d, expected 0..%d\n",
                        idx, sizeof(result)-1));

                return result[idx];
            }
            else
                return result;
        }

        case DINFO_TRACE:
        {
            int which = DIT_CURRENT;

            if (sizeof(args) > 1)
                raise_error("bad number of arguments to debug_info\n");
            if (sizeof(args))
            {
                if (!intp(args[0]))
                    raise_error("bag arg 2 to debug_info().\n");
                which = args[0];
            }

            switch (which)
            {
                case DIT_CURRENT:
                    return efun::driver_info(DI_TRACE_CURRENT);

                case DIT_ERROR:
                    return efun::driver_info(DI_TRACE_LAST_ERROR) || ({ "No trace." });

                case DIT_UNCAUGHT_ERROR:
                    return efun::driver_info(DI_TRACE_LAST_UNCAUGHT_ERROR) || ({ "No trace." });

                case DIT_STR_CURRENT:
                    return efun::driver_info(DI_TRACE_CURRENT_AS_STRING);

                case DIT_CURRENT_DEPTH:
                    return efun::driver_info(DI_TRACE_CURRENT_DEPTH);

                default:
                    raise_error("bad arg 2 to debug_info().\n");
            }

        }

        case DINFO_EVAL_NUMBER:
            return efun::driver_info(DI_EVAL_NUMBER);
    }

    return 0;
}

#endif
