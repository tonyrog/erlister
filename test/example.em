//
// exemple atomic macine
//

machine example;

in x1, x2, x3;

def y1 = x1 && x2;

out z1 = y1 && !state1;

clocks T = atime [0-5.0, 0.2] 2;

states state1, state2, state3;

trans

state1: state2 y1;

state2: state1 !y1 && !x3,
	state3 x3 start(T);

state3: state1 !x3 && timeout(T);
