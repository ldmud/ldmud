string handshake_data;
int handshake;

set_handshake(data) {
    handshake_data = data;
    handshake = random(100000000)+100;
    return handshake;
}

get_handshake(confirm) {
    if (confirm == handshake && handshake) {
        handshake = 0;
	return handshake_data;
    } else {
        return 0;
    }
}
