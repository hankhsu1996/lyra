module Top;
  real a = 5.5;
  real b = 2.0;
  real c;
  initial begin
    c = a - b;
    $display("%f", c);
  end
endmodule
