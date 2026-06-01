module Top;
  typedef union packed {
    logic        [7:0] as_unsigned;
    logic signed [7:0] as_signed;
  } byte_view_t;
  byte_view_t u;
  int result_u;
  int result_s;
  initial begin
    u.as_unsigned = 8'hFF;
    result_u = u.as_unsigned;
    result_s = u.as_signed;
  end
endmodule
