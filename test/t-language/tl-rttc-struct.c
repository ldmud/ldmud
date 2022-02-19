/* Tests the checks for named object types.
 */

#pragma strong_types, save_types, rtt_checks

struct A
{
    int member_a;
};

struct B (A)
{
    int member_b;
};

struct C
{
    int member_c;
};

void rttc_fun_a(struct A var) {}
void rttc_fun_b(struct B var) {}
void rttc_fun_c(struct C var) {}
void rttc_fun_ab(struct A|struct B var) {}
void rttc_fun_abc(struct A|struct B|struct C var) {}
void rttc_fun_any1(struct mixed var) {}
void rttc_fun_any2(struct A|struct mixed var) {}
void rttc_fun_any3(struct B|struct mixed var) {}
void rttc_fun_any4(struct mixed|struct C var) {}

int run_test()
{
    struct A var_a = (<A> 1);
    struct B var_b = (<B> 1, 2);
    struct C var_c = (<C> 3);
    struct mixed any_a = var_a;
    struct mixed any_b = var_b;
    struct mixed any_c = var_c;
    mixed any_int = 42;

    if (catch(rttc_fun_a(var_a);publish) ||
        catch(rttc_fun_a(any_a);publish) ||
        catch(rttc_fun_a(var_b);publish) ||
        catch(rttc_fun_a(any_b);publish) ||
        !catch(rttc_fun_a(any_c);publish) ||
        !catch(rttc_fun_a(any_int);publish))
        return 0;

    if (!catch(rttc_fun_b(any_a);publish) ||
        catch(rttc_fun_b(var_b);publish) ||
        catch(rttc_fun_b(any_b);publish) ||
        !catch(rttc_fun_b(any_c);publish) ||
        !catch(rttc_fun_b(any_int);publish))
        return 0;

    if (!catch(rttc_fun_c(any_a);publish) ||
        !catch(rttc_fun_c(any_b);publish) ||
        catch(rttc_fun_c(var_c);publish) ||
        catch(rttc_fun_c(any_c);publish) ||
        !catch(rttc_fun_c(any_int);publish))
        return 0;

    if (catch(rttc_fun_ab(var_a);publish) ||
        catch(rttc_fun_ab(any_a);publish) ||
        catch(rttc_fun_ab(var_b);publish) ||
        catch(rttc_fun_ab(any_b);publish) ||
        !catch(rttc_fun_ab(any_c);publish) ||
        !catch(rttc_fun_ab(any_int);publish))
        return 0;

    if (catch(rttc_fun_abc(var_a);publish) ||
        catch(rttc_fun_abc(any_a);publish) ||
        catch(rttc_fun_abc(var_b);publish) ||
        catch(rttc_fun_abc(any_b);publish) ||
        catch(rttc_fun_abc(var_c);publish) ||
        catch(rttc_fun_abc(any_c);publish) ||
        !catch(rttc_fun_abc(any_int);publish))
        return 0;

    if (catch(rttc_fun_any1(var_a);publish) ||
        catch(rttc_fun_any1(any_a);publish) ||
        catch(rttc_fun_any1(var_b);publish) ||
        catch(rttc_fun_any1(any_b);publish) ||
        catch(rttc_fun_any1(var_c);publish) ||
        catch(rttc_fun_any1(any_c);publish) ||
        !catch(rttc_fun_any1(any_int);publish))
        return 0;

    if (catch(rttc_fun_any2(var_a);publish) ||
        catch(rttc_fun_any2(any_a);publish) ||
        catch(rttc_fun_any2(var_b);publish) ||
        catch(rttc_fun_any2(any_b);publish) ||
        catch(rttc_fun_any2(var_c);publish) ||
        catch(rttc_fun_any2(any_c);publish) ||
        !catch(rttc_fun_any2(any_int);publish))
        return 0;

    if (catch(rttc_fun_any3(var_a);publish) ||
        catch(rttc_fun_any3(any_a);publish) ||
        catch(rttc_fun_any3(var_b);publish) ||
        catch(rttc_fun_any3(any_b);publish) ||
        catch(rttc_fun_any3(var_c);publish) ||
        catch(rttc_fun_any3(any_c);publish) ||
        !catch(rttc_fun_any3(any_int);publish))
        return 0;

    if (catch(rttc_fun_any4(var_a);publish) ||
        catch(rttc_fun_any4(any_a);publish) ||
        catch(rttc_fun_any4(var_b);publish) ||
        catch(rttc_fun_any4(any_b);publish) ||
        catch(rttc_fun_any4(var_c);publish) ||
        catch(rttc_fun_any4(any_c);publish) ||
        !catch(rttc_fun_any4(any_int);publish))
        return 0;

    return 1;
}
