module Top;
  real r = 3.14159265358979;
  shortreal s;
  initial begin
    s = r;
    $display("%f", s);
  end
endmodule
