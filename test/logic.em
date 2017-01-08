
machine LOGIC;

in x1 = a && b || c,
   x2 = SOME x have_x_resource(x) || d;

out 
    y1 = x1,
    y2 = x2;

states OFF, ON;

trans

OFF: ON x1;

ON: OFF x2;


      