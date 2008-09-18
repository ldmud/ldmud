/*---------------------------------------------------------------------------
 * Eval Cost functions.
 *
 *---------------------------------------------------------------------------
 */

#ifndef I_EVAL_COST__
#define I_EVAL_COST__

#include "driver.h"

#include "interpret.h"

/*-------------------------------------------------------------------------*/
static INLINE Bool
add_eval_cost_n (int32 added_cost, uint repetitions)

/* Increase the evaluation cost by <added_cost>*<repetitions>. Return TRUE if
 * this would exceed max_eval_cost, return FALSE if not.
 *
 * Passing <repetitions> explicitely guards against situations where
 * <added_cost>*<repetitions> itself would overflow.
 *
 * To safeguard the driver against ridiculous executions, the actual eval_cost
 * is capped at max_eval_cost+1.
 */

{
    if (repetitions == 0)
        return MY_FALSE;

    if (max_eval_cost)
    {
        /* Test the evaluation cost against the limit.
         * eval_cost < 0 signify a wrap-around - unlikely, but with these crazy
         * wizards everything is possible.
         */
        if (eval_cost >= max_eval_cost || eval_cost < 0)
            return MY_TRUE;

        if (max_eval_cost - eval_cost < added_cost
         || (int32)((max_eval_cost - eval_cost) / repetitions) < added_cost
           )
        {
            total_evalcost += max_eval_cost - eval_cost + 1;
            eval_cost = max_eval_cost+1;
            return MY_TRUE;
        }
    }

    eval_cost += added_cost * repetitions;
    total_evalcost += added_cost * repetitions;

    return MY_FALSE;
} /* add_eval_cost_n() */

/*-------------------------------------------------------------------------*/

/* --- Macros --- */

/* Increase the eval cost for a non-repetitive action.
 */
#define add_eval_cost(cost) add_eval_cost_n(cost, 1)

/* Reset the evaluation cost/time counter.
 */
#define CLEAR_EVAL_COST (assigned_eval_cost = eval_cost = 0)

/* Check if the current evaluation took too long
 */
#define EVALUATION_TOO_LONG() \
    (max_eval_cost && (eval_cost >= max_eval_cost || eval_cost < 0))

/* Return the amount of remaining evaluation costs, or MAX_INT if there
 * is no real maximum.
 */
#define GET_REMAINING_EVAL_COST() \
    ( max_eval_cost ? ((eval_cost >= max_eval_cost || eval_cost < 0) ? 0 : (max_eval_cost - eval_cost)) : INT32_MAX)

/***************************************************************************/

#endif /* I_EVAL_COST__ */
