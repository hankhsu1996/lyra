module Top;
  reg [7:0] r;
  initial begin
    r = 8'bxxzz0011;
    $display("%b", r);
  end
endmodule
