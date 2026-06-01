module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int result;
  initial begin
    s = 16'hFFFF;
    s.a = 8'h00;
    result = s;
  end
endmodule
