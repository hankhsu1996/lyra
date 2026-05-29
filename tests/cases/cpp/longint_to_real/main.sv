module Top;
  longint x;
  real r;
  initial begin
    x = 1000000;
    r = x;
    $display("%f", r);
  end
endmodule
