module Top;
  real a = 2.5;
  shortreal b = 4.0;
  real c;
  initial begin
    c = a * b;
    $display("%f", c);
  end
endmodule
