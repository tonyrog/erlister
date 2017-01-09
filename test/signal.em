//
// Composed Machine example
//

machine SIGNAL;

submachines GREEN, RED;

in goahead, lg, lr;

out g = GREEN.on,
    r = RED.on;

submachine GREEN;

in request_on = goahead,
   light_control = lg,
   error_in = RED.error ;

out ok = request_on -> light_control,
    error = request_on && !light_control &&
    timeout(T1) && !error_in;

clocks T1 = gtime [0-2.0, 0.2] 1.0;

states off, on;

trans

off: on request_on && !error_in start(T1);

on: off !request_on || error_in;


submachine RED;

in request_on = !goahead,
   light_control = lr,
   error_in = GREEN.error,
   gok = GREEN.ok;

out error = off && light_control && timeout(T2) && !error_in;

clocks T2 = rtime [0-2.0, 0.2] 1.0;

states on, off;

trans

off:  on request_on || error_in;
on:  off !request_on && gok start(T2);
