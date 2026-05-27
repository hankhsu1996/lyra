module Top;
  logic [3:0] a;
  logic [3:0] b;
  int         eq_same;
  int         eq_diff;
  int         eq_xz;
  int         neq_same;
  initial begin
    a = 4'b10x1;
    b = 4'b10x1;
    eq_same = (a === b) ? 1 : 0;
    neq_same = (a !== b) ? 1 : 0;

    b = 4'b1011;
    eq_diff = (a === b) ? 1 : 0;

    a = 4'bxxxx;
    b = 4'bxxxx;
    eq_xz = (a === b) ? 1 : 0;
  end
endmodule
