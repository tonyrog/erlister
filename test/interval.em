//
// INTERVAL function button (toggle)
//

machine interval;

submachines alternate, springback;

in button;

out value = springback.value && alternate.value;

submachine springback;

in button = button;

out value = ON;

states OFF, ON;

trans

OFF: ON  button;
ON:  OFF button;

submachine alternate;

in enable = springback.value;

out value = HIGH;

clocks Th = high_time [0-5.0, 0.2] 2,
       Tl = low_time  [0-5.0, 0.2] 2;

states LOW, HIGH;

trans

LOW:  HIGH enable && timeout(Th) start(Tl);
HIGH: LOW  enable && timeout(Tl) start(Th);
