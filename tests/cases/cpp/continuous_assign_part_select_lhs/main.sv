module Top;
  bit [7:0] x;
  bit [3:0] y;

  initial begin
    x = 8'ha0;
    y = 4'h5;
  end

  // LRM 10.3 Table 10-1 allows a constant part-select LHS in a continuous
  // assignment; the low nibble of `x` tracks `y` continuously.
  assign x[3:0] = y;
endmodule
