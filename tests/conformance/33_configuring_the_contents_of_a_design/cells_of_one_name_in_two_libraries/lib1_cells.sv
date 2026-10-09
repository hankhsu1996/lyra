module A;
  int which = 1;
endmodule

module B;
  int which = 1;
endmodule

module Binder;
  bind TC.t1 Probe x (.d(8'd1));
endmodule
