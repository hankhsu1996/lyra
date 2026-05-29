module Top;
  byte b;
  real r;
  initial begin
    b = 50;
    r = b;
    $display("%f", r);
  end
endmodule
