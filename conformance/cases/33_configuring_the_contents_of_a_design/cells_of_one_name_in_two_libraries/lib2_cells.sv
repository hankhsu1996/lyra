module A;
  int which = 2;
endmodule

module B;
  int which = 2;
endmodule

module Binder;
  bind TC.t2 Probe x (.d(8'd2));
endmodule
