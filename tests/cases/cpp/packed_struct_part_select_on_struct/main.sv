module Top;
  typedef struct packed {
    logic [7:0] a;
    logic [7:0] b;
  } my_struct_t;
  my_struct_t s;
  int low_byte;
  int high_byte;
  int low_indexed;
  int high_indexed;
  initial begin
    s = 16'hABCD;
    low_byte = s[7:0];
    high_byte = s[15:8];
    low_indexed = s[0 +: 8];
    high_indexed = s[15 -: 8];
  end
endmodule
