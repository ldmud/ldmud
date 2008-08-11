int logon(int flag)
{
    enable_telnet(0);
    set_prompt("");
    
    ed("/dummy","ed_ends");
    return 1;
}

void ed_ends()
{
    rm("/dummy");
}
