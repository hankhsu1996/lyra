module Top;
  typedef union packed {
    logic [15:0] a;
    logic [15:0] b;
  } my_union_t;
  my_union_t u1;
  my_union_t u2;
  int result;
  initial begin
    u1.a = 16'hBEEF;
    u2 = u1;
    result = u2.b;
  end
endmodule
