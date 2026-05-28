module Top;
  real a = 7.5;
  real b = 2.5;
  real c;
  initial begin
    c = a / b;
    $display("%f", c);
  end
endmodule
