module Top;
  typedef union packed {
    logic [15:0] as_logic;
    bit [15:0]   as_bit;
  } pun_t;
  pun_t u;
  int result;
  initial begin
    u.as_logic = 16'hDEAD;
    result = u.as_bit;
  end
endmodule
