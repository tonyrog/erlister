// -*- c -*-

machine arithmetic;

boolean    in x1;
unsigned8  in x2;
unsigned16 in x3;
unsigned32 in x4;
integer8   in x5;
integer16  in x6;
integer32  in x7;

boolean    def y1 = x1 && true;
unsigned8  def y2 = x2*2;
unsigned16 def y3 = x3-3;
unsigned32 def y4 = x4/4;
integer8   def y5 = x5%5;
integer16  def y6 = x6==7;
integer32  def y7 = x7 && (x1);

boolean    out z1 = y1;
unsigned8  out z2 = y2;
unsigned16 out z3 = y3;
unsigned32 out z4 = y4;
integer8   out z5 = y5;
integer16  out z6 = y6;
integer32  out z7 = y7;
