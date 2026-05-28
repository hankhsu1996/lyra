module Top;
  shortreal a = 2.0;
  shortreal b = 3.0;
  shortreal c;
  initial begin
    c = a ** b;
    $display("%f", c);
  end
endmodule
