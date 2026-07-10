module Top;
  import "DPI-C" function real scale(input real r, input int k);
  import "DPI-C" function real negate(input real r);
  real a;
  initial begin
    a = scale(1.5, 4);
    $display("scale=%0.1f", a);
    a = negate(2.5);
    $display("negate=%0.1f", a);
  end
endmodule
