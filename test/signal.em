//
// Composed Machine example
//

machine SIGNAL;

submachines GREEN, RED;

in goahead, lg, lr;

out g = GREEN.on,
    r = RED.on;

submachine GREEN;

in want_on = goahead,
   light_control = lg,
   error_in = RED.error ;

out ok = want_on -> light_control,
    error = want_on && !light_control &&
    timeout(T1) && !error_in;

clocks T1 = gtime [0-2.0, 0.2] 1.0;

states OFF, ON;

trans

OFF: ON wish_on && !error_in start(T1);

ON: OFF !wish_on || error_in;


submachine RED;

in wish_on = !goahead,
   light_control = lr,
   error_in = GREEN.error,
   gok = GREEN.ok;

out error = OFF && light_control && timeout(T2) && !error_in;

clocks T2 = rtime [0-2.0, 0.2] 1.0;

states ON, OFF;

trans

OFF:  ON wish_on || error_on;
ON:  OFF !wish_on && gok start(T2);
