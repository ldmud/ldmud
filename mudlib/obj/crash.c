/*
 * Just make a noisy entrance for a wizard.
 */
reset() {
    shout("You hear a distant rumble.\n" +
	call_other(this_player(), "query_name") +
	" has entered the game.\n");
    set_heart_beat(1);
}

heart_beat() {
    destruct(this_object());
}
