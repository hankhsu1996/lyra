module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int result;
  initial begin
    s = 0;
    s.a = 8'hAA;
    s.b = 8'hBB;
    result = s;
  end
endmodule
