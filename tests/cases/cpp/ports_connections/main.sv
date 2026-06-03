module Src(output int d);
  initial d = 42;
endmodule

module Snk(input int d);
  int cap;
  always_comb cap = d;
endmodule

module Inner(input int x);
  int y;
  always_comb y = x + 1;
endmodule

module Outer(input int a);
  int local_s;
  Inner inner(.x(a));
  always_comb local_s = 100;
endmodule

module Top;
  int link;
  Src s(.d(link));
  Snk k(.d(link));
  int w;
  Outer outer(.a(w));
  initial begin
    w = 41;
    #1;
    $display("%0d %0d %0d %0d", link, k.cap, outer.inner.y, outer.local_s);
  end
endmodule
