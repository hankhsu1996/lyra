module Top;
  shortreal s = 3.14;
  real r;
  initial begin
    r = s;
    $display("%f", r);
  end
endmodule
