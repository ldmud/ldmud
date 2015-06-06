What happens when an inline closure is parsed
=============================================

This short document tries to outline how scopes and variables are
handled when parsing inline closures.

Let's take the following closure as an example:

    int var;

    function void(string arg) : int ctx = 42
    {
        int i = 2;

        ctx += var - i;
    };

This closure might be a part of a code block, an initializer of a global
variable or an initializer of a context variable of another closure.

The example above has 4 block scopes in this order:

  1) The outer block scope with 'var'.
  2) The context scope with 'ctx'.
  3) The argument scope with 'arg'.
  4) The inner block scope with 'i'.

In each scope there may be three kinds of local variables
(each one of them has .type = I_TYPE_LOCAL):
  * real local ones:            .u.local.context < 0
  * explicit context variables: .u.local.context >= 0 and .u.local.num < 0
  * implicit context variables: .u.local.context >= 0 and .u.local.num >= 0

Each local variable will have an index on the stack relative to the stack
frame of the function or inline closure, and each context variable will
have an index into the closure's context. So the variable counters will
be reset in block 2 (context scope), because then the closure (with it's
own stack frame) will start. The block 2 (context) is first compiled as a
part of the outer function and will then be transferred to the closure.

The above example will introduce the following variables:

  Outer function:
    'var': A local variable in scope 1, index 0
    'ctx': A local variable in scope 2, index 1

  Inline closure:
    'arg': A local variable in scope 3, index 0
    'i':   A local variable in scope 4, index 1
    'ctx': An explicit context variable in scope 2, context index 0
    'var': An implicit context variable in scope 2, context index 1,
                                                   variable index 1.

The inline closure is parsed in 4 steps:

  a) After 'function void' the inline closure information structure
     (inline_closure_t) is initialized. For each inline closure
     that was parsed within a normal function such a structure is
     put into the A_INLINE_CLOSURE memory area. When the normal
     function is finished, the program code of all these closures
     will be copied from these structures into the real program's
     code block.

     Also, the context scope and the argument scope are initialized.
     The argument scope is the inner scope, because they are parsed
     next. The variable counter (current_number_of_locals) is reset
     to 0 (but the old value is saved in the scope structure).

  b) Then (string arg) will be parsed, the same as for normal functions.
     The variable names will be put into the innermost scope as local names.

  c) The context ': int ctx = 42' will be parsed. The context arguments
     will be parsed as if they would belong to the outer scope. In fact,
     the compiler will issue bytecode to initialize them as local
     variables of the outer function and store their values there.
     The F_CONTEXT_CLOSURE bytecode will later copy these variables
     from this function into the closure as its context.

     In order for this to happen, scope 2 is initialized as it would
     belong to the outer function (its variable counter 'first_local'
     is set to the counter of scope 1). Scope 3 will be ignored and
     marked inaccessible so that references to argument names are
     caught as errors. Then the context is parsed and the variables
     are not put into the topmost scope 3, but the one underneath
     it (scope 2).

     In the end, each variable in scope 2 is given a context index
     and stripped of its variable index, so that they become explicit
     context variables.

  d) The block will be parsed. That's done just like a normal function
     except that, wherever a local variable is used, the compiler calls
     check_for_context_local to see whether this variable belongs
     to an outer scope (block depth of the variable is below
     the initial block depth of the inline closure). If that is the
     case the compiler introduces a new context variable with the same
     name but in the context scope 2 of the inline variable. The
     index of the original variable is recorded in the .u.local.num
     field (it the original variable is also a context variable,
     CONTEXT_VARIABLE_BASE is added to its context index).

     After the block has been compiled, its bytecode will be moved
     into the A_INLINE_CLOSURE memory block (together with the
     line-number information). It will be copied back after the
     current function has been completed. Also, a bytecode is
     issued for each implicit variable to put the value from the
     original variable on the stack. These values will serve
     as arguments for the final F_CONTEXT_CLOSURE bytecode.
