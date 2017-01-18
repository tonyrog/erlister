// C code generated from example.em

typedef unsigned char  digital_t;
typedef unsigned short analog_t;
typedef unsigned long  timer_t;
typedef unsigned char  state_t;

#define EXAMPLE_x1 0
#define EXAMPLE_x2 1
#define EXAMPLE_x3 2
#define EXAMPLE_T  0

digital_t input[3];
digital_t output[1];
timer_t   clocks[1];
state_t   state;

void start(int i)
{
    clocks[i] = tick();
}

int timeout(int i)
{
    if (clock[i]+2000 >= tick())
	return 1;
    return 0;
}

void machine()
{
    digital y1 = input[EXAMPLE_x1] && input[EXAMPLE_x2];

    switch(state) {
    case state1:
	if (!y1 && !input[EXAMPLE_x3])
	    state = state2;
	else if (!input[EXAMPLE_x2] && timeout(EXAMPLE_T))
	    state = state3;
	break;
    case state2:
	if (y1)
	    state = state1;
	break;
    case state3:
	if (input[EXAMPLE_x3]) {
	    start(EXAMPLE_T);
	    state = state2;
	}
	break;
    }
}
