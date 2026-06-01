module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int upper_a;
  int lower_a;
  int bit7_b;
  int bit0_b;
  initial begin
    s = 16'hF00F;
    upper_a = s.a[7:4];
    lower_a = s.a[3:0];
    bit7_b = s.b[7];
    bit0_b = s.b[0];
  end
endmodule
