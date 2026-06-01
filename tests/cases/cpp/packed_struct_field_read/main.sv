module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int result_a;
  int result_b;
  int sum;
  initial begin
    s = 16'hAABB;
    result_a = s.a;
    result_b = s.b;
    sum = s.a + s.b;
  end
endmodule
