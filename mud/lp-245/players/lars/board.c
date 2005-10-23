#define EMPTY	0
#define BLACK	1
#define WHITE	2

int num_pass;
mixed * board, *mark;

object black, white;
int to_play;

int occupied() {
    if (to_play == EMPTY)
	return 0;
    if (black && present(black, environment(this_object()))) {
	write("You have to wait for " + call_other(black, "query_name") +
	      ".\n");
	return 1;
    }
    if (white && present(white, environment(this_object()))) {
	write("You have to wait for " + call_other(white, "query_name") +
	      ".\n");
	return 1;
    }
    return 0;
}

void display(mixed * b) {
    int x, y;

    if (to_play == BLACK)
	write("Black '@' (" + black->query_name() + ") to play.\n\n");
    else
	write("White 'O' (" + white->query_name() + ") to play.\n\n");
    write("  0 1 2 3 4 5 6 7 8\n");
    while(x<9) {
	write(x + " ");
	while(y<9) {
	    if (b[x][y] == EMPTY)
		write(". ");
	    else if (b[x][y] == BLACK)
		write("@ ");
	    else if (b[x][y] == WHITE)
		write("O ");
	    y += 1;
	}
	write("\n");
	x += 1;
	y = 0;
    }
}

void notify(string str) {
    say(this_player()->query_name() + " " + str);
}

string short() {
    return "board";
}

void long() {
    write("A go board. If you want to play\n");
    write("with someone, do 'start name' with the name of your friend.\n");
    write("The player issuing the 'start' command will get black.\n");
    write("An optional numeric argument to start is the number of handicap stones.\n");
    if (black || white) {
	write("\n");
	display(board);
    }
}

int id(string str) {
    return str == "board" || str == "go board";
}

int query_value() { return 10; }

int get() {
    return !occupied();
}

void reset(int arg) {
    if (arg)
	return;
    to_play = EMPTY;
}

void place_hand(int n) {
    board[2][2] = BLACK;
    board[6][6] = BLACK;
    if (n < 3)
	return;
    board[2][6] = BLACK;
    if (n < 4)
	return;
    board[6][2] = BLACK;
    if (n < 5)
	return;
    board[4][4] = BLACK;
    if (n < 6)
	return;
    board[4][4] = EMPTY;
    board[4][2] = BLACK;
    board[4][6] = BLACK;
    if (n < 7)
	return;
    board[4][4] = BLACK;
    if (n < 8)
	return;
    board[4][4] = EMPTY;
    board[2][4] = BLACK;
    board[6][4] = BLACK;
    if (n < 9)
	return;
    board[4][4] = BLACK;
}

int start(string str) {
    int i, handicap;
    mixed name;
    if (environment() == this_player()) {
	write("You mus put it down first !\n");
	return 1;
    }
    if (sscanf(str, "%s %d", name, handicap) != 2)
	name = str;
    name = present(name, environment(this_object()));
    if (!name)
	return 0;
    if (occupied())
	return 1;
/*
    if (name == this_player()) {
	write("Sorry, you can't play against yourself.\n");
	return 1;
    }
*/
    notify(" starts a game with " + call_other(name, "query_name") + ".\n");
    to_play = BLACK;
    num_pass = 0;
    white = name;
    black = this_player();
    board = allocate(9);
    mark = allocate(9);
    while(i<9) {
	board[i] = allocate(9);
	mark[i] = allocate(9);
	i += 1;
    }
    write("Board initialized.\n");
    if (handicap) {
	if (handicap < 2 || handicap > 9) {
	    write("The handicap must be from 2 to 9 stones.\n");
	    white = 0;
	    black = 0;
	    return 1;
	}
	write("You get " + handicap + " stones, and white starts.\n");
	notify(" gets " + handicap + " stones, and white starts.\n");
	place_hand(handicap);
	to_play = WHITE;
    }
    return 1;
}

int disp() {
    display(board);
    return 1;
}

void init() {
    add_action("disp", "disp");
    add_action("start", "start");
    add_action("patch", "patch");
    add_action("play", "play");
    add_action("score", "score");
    add_action("fill", "fill");
}

void clean(mixed * b) {
    int i, j;

    i = 0;
    while(i<9) {
	j = 0;
	while(j<9) {
	    b[i][j] = 0;
	    j += 1;
	}
	i += 1;
    }
}

int lib(mixed *b, mixed *m, int x, int y) {
    int col, sum;

    sum = 0;
    col = b[x][y];
    m[x][y] = 1;
    if (x > 0 && !m[x-1][y]) {
	if (b[x-1][y] == col)
	    sum += lib(b, m, x-1, y);
	else if (b[x-1][y] == EMPTY) {
	    sum += 1;
	    m[x-1][y] = 1;
	}
    }
    if (y > 0 && !m[x][y-1]) {
	if (b[x][y-1] == col)
	    sum += lib(b, m, x, y-1);
	else if (b[x][y-1] == EMPTY) {
	    sum += 1;
	    m[x][y-1] = 1;
	}
    }
    if (x < 8 && !m[x+1][y]) {
	if (b[x+1][y] == col)
	    sum += lib(b, m, x+1, y);
	else if (b[x+1][y] == EMPTY) {
	    sum += 1;
	    m[x+1][y] = 1;
	}
    }
    if (y < 8 && !m[x][y+1]) {
	if (b[x][y+1] == col)
	    sum += lib(b, m, x, y+1);
	else if (b[x][y+1] == EMPTY) {
	    sum += 1;
	    m[x][y+1] = 1;
	}
    }
    return sum;
}

int count_lib(mixed * b, int x, int y) {
    clean(mark);
    return lib(b, mark, x, y);
}

int rem_stones(mixed * b, int x, int y) {
    int col, sum;

    col = b[x][y];
    b[x][y] = EMPTY;
    sum = 1;
    if (x>0 && b[x-1][y] == col)
	sum += rem_stones(b, x-1, y);
    if (y>0 && b[x][y-1] == col)
	sum += rem_stones(b, x, y-1);
    if (x<8 && b[x+1][y] == col)
	sum += rem_stones(b, x+1, y);
    if (y<8 && b[x][y+1] == col)
	sum += rem_stones(b, x, y+1);
    return sum;
}

int enter(mixed * b, int x, int y, int col) {
    int ocol, sum;
    b[x][y] = col;
    if (col == BLACK)
	ocol = WHITE;
    else
	ocol = BLACK;
    if (x>0 && b[x-1][y] == ocol && count_lib(b, x-1, y) == 0)
	sum += rem_stones(b, x-1, y);
    if (y>0 && b[x][y-1] == ocol && count_lib(b, x, y-1) == 0)
	sum += rem_stones(b, x, y-1);
    if (x<8 && b[x+1][y] == ocol && count_lib(b, x+1, y) == 0)
	sum += rem_stones(b, x+1, y);
    if (y<8 && b[x][y+1] == ocol && count_lib(b, x, y+1) == 0)
	sum += rem_stones(b, x, y+1);
    return sum;
}

int patch(string str) {
    int x, y;
    mixed col;
    int ret;

    if (!str)
	return 0;
    if (sscanf(str, "%d %d %s", x, y, col) != 3)
	return 0;
    if (col == "@")
	col = BLACK;
    else if (col == "O")
	col = WHITE;
    else if (col == ".")
	col = EMPTY;
    else
	return 0;
    if (col != EMPTY) {
	ret = enter(board, x, y, col);
	board[x][y] = col;
    } else
	board[x][y] = EMPTY;
    write("Ok.\n");
    return 1;
}

int play(string str) {
    int x, y;
    int ret;

    if (!str)
	return 0;
    if (to_play == EMPTY) {
	write("You have to start the game with an opponent first.\n");
	return 1;
    }
    if (to_play == BLACK && this_player() != black) {
	write("It is not your turn now.\n");
	notify("tried (illegaly) to play.\n");
	return 1;
    }
    if (to_play == WHITE && this_player() != white) {
	write("It is not your turn now.\n");
	notify("tried (illegaly) to play.\n");
	return 1;
    }
    if (str == "pass") {
	num_pass += 1;
	notify("pass.\n");
	if (num_pass == 2) {
	    write("Game over.\n");
	    say("Game over.\n");
	    to_play = EMPTY;
	    return 1;
	}
	if (to_play == BLACK)
	    to_play = WHITE;
	else
	    to_play = BLACK;
	write("Ok.\n");
	return 1;
    }
    num_pass = 0;
    if (sscanf(str, "%d %d", x, y) != 2)
	return 0;
    notify("playes at " + x + " " + y + ".\n");
    ret = enter(board, x, y, to_play);
    if (ret) {
	write(ret + " stones removed, ");
	say("And captures " + ret + " stones.\n");
    }
    write(count_lib(board, x, y) + " liberties.\n");
    if (to_play == BLACK)
	to_play = WHITE;
    else
	to_play = BLACK;
    return 1;
}

int count_spaces2(mixed *b, int x, int y, int col)
{
    int tot;

    if (x < 0 || x > 8 || y < 0 || y > 8)
	return 0;
    if (b[x][y] == EMPTY) {
	if (mark[x][y])
	    return 0;
	mark[x][y] = 1;
	tot = 1;
    } else if (b[x][y] == col)
	return 0;
    else
	return 2000;
    write(x + " " + y + "\n");
    tot += count_spaces2(b, x-1, y, col);
    tot += count_spaces2(b, x+1, y, col);
    tot += count_spaces2(b, x, y-1, col);
    tot += count_spaces2(b, x, y+1, col);
    return tot;
}

int count_spaces(mixed * b, int x, int y, int col)
{
    int tot;
    tot = count_spaces2(b, x, y, col);
    if (col == WHITE)
	tot = -tot;
    return tot;
}

int count_score(mixed *b) {
    int x, y;
    int score;
    int col, tmp;

    clean(mark);
    while(x<9) {
	y = 0;
	while(y<9) {
	    col = b[x][y];
	    if (col == EMPTY) {
		y += 1;
		continue;
	    }
	    if (col == BLACK)
		score += 1;
	    else
		score -= 1;
	    tmp = count_spaces(b, x-1, y, col);
	    if (tmp < 1000 && tmp > -1000)
		score += tmp;
	    tmp = count_spaces(b, x+1, y, col);
	    if (tmp < 1000 && tmp > -1000)
		score += tmp;
	    tmp = count_spaces(b, x, y-1, col);
	    if (tmp < 1000 && tmp > -1000)
		score += tmp;
	    tmp = count_spaces(b, x, y+1, col);
	    if (tmp < 1000 && tmp > -1000)
		score += tmp;
	    y += 1;
	}
	x += 1;
    }
    return score;
}

void chose_random() {
    int x, y, n;
    while(n < 10) {
	x = random(9);
	y = random(9);
	if (board[x][y] == EMPTY) {
	    int ret;
	    board[x][y] = BLACK;
	    if (count_lib(board, x, y) == 0) {
		n += 1;
		continue;
	    }
	    n = enter(board, x, y, BLACK);
	    write("I play on " + x + " " + y);
	    if (ret)
		write("And removes " + ret + " stones!");
	    write("\n");
	    return;
	}
	n += 1;
    }
    write("I pass.\n");
}

int score() {
    int tmp;

    write("The score is now ");
    tmp = count_score(board);
    if (tmp == 0)
	write("even.\n");
    else if (tmp > 0)
	write(tmp + " points to black.\n");
    else
	write(-tmp + " points to white.\n");
    return 1;
}

int ch(mixed * b, int x, int y, int col) {
    if (x < 0 || x > 8 || y < 0 || y > 8)
	return 1;
    if (b[x][y] == EMPTY || b[x][y] == col)
	return 1;
    return 0;
}

int fill_point(mixed *b, int x, int y, int col)
{
    if (board[x][y] != EMPTY)
	return 0;
    if (ch(b,x-1,y,col) && ch(b,x,y-1,col) &&
	ch(b,x+1,y,col) && ch(b,x,y+1,col)) {
	board[x][y] = col;
	return 1;
    }
    return 0;
}

int fill() {
    int x, y;
    int tot;
    mixed * b;
    b = board;
    board = allocate(9);
    while(x<9) {
	board[x] = allocate(9);
	x += 1;
    }
    x = 0;
    while(x<7) {
	x += 1;
	y = 0;
	while(y<7) {
	    int col;
	    y += 1;
	    col = b[x][y];
	    board[x][y] = col;
	    if (col == EMPTY)
		continue;
	    tot += fill_point(b, x-1, y, col);
	    tot += fill_point(b, x, y-1, col);
	    tot += fill_point(b, x, y+1, col);
	    tot += fill_point(b, x+1, y, col);
	}
    }
    if (tot > 0)
	write(tot + " spaces filled up.\n");
    else
	write("No more free spaces.\n");
    return 1;
}

