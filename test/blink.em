machine blink;

clocks Ton = on_time [0-5, 0.01]  0.500;
clocks Toff = off_time [0-5, 0.01] 0.750;

states init, on, off;

trans

init: off true start(Toff) { gpio_output(13); gpio_clr(13); uart_send(48);  }
      ;

off: on timeout(Toff) start(Ton) { gpio_set(13); uart_send(49); }
     ;

on: off timeout(Ton) start(Toff) { gpio_clr(13); uart_send(48); }
    ;

