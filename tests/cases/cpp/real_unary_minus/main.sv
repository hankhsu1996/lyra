module Top;
  real a = 3.5;
  real b;
  initial begin
    b = -a;
    $display("%f", b);
  end
endmodule
