module Leaf;
  int x;
  always_comb x = Top.g;
endmodule

module Top;
  int g;
  Leaf l();
endmodule

module Tb;
  Top dut();
  initial begin
    dut.g = 42;
    #1;
    $display("%0d", dut.l.x);
  end
endmodule
