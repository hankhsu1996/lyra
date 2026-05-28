module Top;
  shortreal a = 1.5;
  shortreal b = 2.5;
  shortreal c;
  initial begin
    c = a + b;
    $display("%f", c);
  end
endmodule
