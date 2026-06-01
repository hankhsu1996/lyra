module Top;
  typedef union soft packed {
    logic [15:0] wide;
    logic [7:0]  narrow;
  } u_t;
  u_t u;
  int result;
  initial begin
    u.wide = 16'hFF00;
    u.narrow = 8'hAB;
    result = u.wide;
  end
endmodule
