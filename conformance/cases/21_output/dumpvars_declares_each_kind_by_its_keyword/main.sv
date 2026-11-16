// A dump declares each variable and net by the keyword of its kind and the
// bits it holds (LRM 21.7.2.1 `var_type`). A variable of a SystemVerilog type
// masquerades as the IEEE 1364 type Table 21-11 maps it to: a bit or logic
// vector, a packed array, a packed structure and a packed union as one reg of
// all their bits, an int as an integer of 32, an enumeration as its base type,
// a shortint, a longint and a byte as a reg of 16, 64 and 8, and a shortreal as
// a real (LRM 21.7.5). An integer, a time, a real, a realtime and an event keep
// their own keywords, a port is declared as the variable or net it connects,
// and a net is declared by its net type, a uwire as a wire (LRM 21.7.2.3).
// The dump the run writes is held to the one in expected/, read as the
// standard reads a dump.
`timescale 1ns / 1ns
module Top (
    input var event p
);
  bit b = 1'b1;
  logic [7:0] l = 8'h5a;
  logic [1:0][3:0] m = 8'h3c;
  struct packed {
    logic [2:0] x;
    bit y;
  } s = 4'b1011;
  union packed {
    logic [3:0] a;
    bit [3:0] b;
  } pu = 4'b0110;
  int i = 5;
  shortint si = 6;
  longint li = 7;
  byte by = 8;
  integer n = 9;
  time t = 10;
  real r = 1.5;
  shortreal sr = 2.5;
  realtime rt = 3.5;
  enum {
    RED,
    GREEN
  } c = GREEN;
  enum logic [3:0] {
    IDLE = 4'd3,
    RUN
  } e4 = RUN;
  event ev;
  wire w = b;
  tri tr = b;
  wand wa = b;
  triand ta = b;
  wor wo = b;
  trior tor = b;
  tri0 t0;
  tri1 t1;
  supply0 s0;
  supply1 s1;
  uwire u = b;
  trireg tg;

  initial begin
    $dumpfile("kinds.vcd");
    $dumpvars;
    #1 $display("All checks passed");
  end
endmodule
