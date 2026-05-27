module Top;
  logic [3:0] a;
  logic [3:0] b;
  int         r;
  initial begin
    r = (a ==? b) ? 1 : 0;
  end
endmodule
