module Top;
  int i;
  real r;
  initial begin
    i = 42;
    r = i;
    $display("%f", r);
  end
endmodule
