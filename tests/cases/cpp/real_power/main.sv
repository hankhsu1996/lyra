module Top;
  real a = 2.0;
  real b = 3.0;
  real c;
  initial begin
    c = a ** b;
    $display("%f", c);
  end
endmodule
