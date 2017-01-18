
machine LOGIC;

in x1, x2,
   x3 = SOME x have_x_resource(x) && (x1 || x2);

out 
    y1 = x1,
    y2 = x2;

states OFF, ON;

trans

OFF: ON x1;

ON: OFF x2;


      