module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s1;
  my_struct_t s2;
  int result_a;
  int result_b;
  int result_total;
  initial begin
    s1.a = 8'hAA;
    s1.b = 8'hBB;
    s2 = s1;
    result_a = s2.a;
    result_b = s2.b;
    result_total = s2;
  end
endmodule
