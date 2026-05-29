module Top;
  int i;
  shortreal r;
  initial begin
    i = 42;
    r = i;
    $display("%f", r);
  end
endmodule
