module Top;
  int a, b, c;
  initial begin
    a = 30; b = 12;
    c = a + b; $display("%0d", c);
    c = a - b; $display("%0d", c);
    c = b - a; $display("%0d", c);
    a = 5; b = 7;
    c = a * b; $display("%0d", c);
    a = b - 12;
    c = a * b; $display("%0d", c);
    a = 100; b = 4;
    c = a / b; $display("%0d", c);
    a = b - 104;
    c = a / b; $display("%0d", c);
    a = 17; b = 5;
    c = a % b; $display("%0d", c);
  end
endmodule
