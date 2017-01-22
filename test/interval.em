//
// INTERVAL function button (toggle)
//

machine interval;

submachines springback, alternate;

in button;

out value = springback.value && alternate.value;

submachine springback;

in button = button;

out value = on;

states off, on;

trans

off: on  button;
on:  off button;

submachine alternate;

in enable = springback.value;

out value = high;

clocks Th = high_time [0-5, 0.2] 2,
       Tl = low_time  [0-5, 0.2] 2;

states low, high;

trans

low:  high enable && timeout(Th) start(Tl);
high: low  enable && timeout(Tl) start(Th);
