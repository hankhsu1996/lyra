module Top;
  real a = 2.0;
  real b = 3.0;
  real c = 4.0;
  real result;
  initial begin
    result = (a + b) * c;
    $display("%f", result);
  end
endmodule
