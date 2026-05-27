module Top;
  bit msb;
  bit near_msb;
  int via_int;
  initial begin
    bit signed [7:0] s;
    s = 8'sb1000_0001;
    msb = s[7];
    near_msb = s[6];
    via_int = s[7];
  end
endmodule
