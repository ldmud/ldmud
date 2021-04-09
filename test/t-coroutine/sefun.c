#pragma save_types, strong_types, rtt_checks

async void sleep1(int sec)
{
    call_out(#'call_coroutine, sec, this_coroutine());

    yield();
}

coroutine sleep2(int sec)
{
    coroutine result = async function void() { yield(); };

    call_out(#'call_coroutine, sec, result);

    return result;
}
