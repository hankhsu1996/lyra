module Top;
  logic [7:0] x;
  real r;
  initial begin
    x = 8'b1xxx_0001;
    r = x;
    $display("%f", r);
  end
endmodule
