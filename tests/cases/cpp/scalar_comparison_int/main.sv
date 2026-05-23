module Top;
  int a, b, c;
  initial begin
    a = 5; b = 10;
    c = (a == b); $display("%0d", c);
    c = (a != b); $display("%0d", c);
    c = (a < b);  $display("%0d", c);
    c = (a <= b); $display("%0d", c);
    c = (a > b);  $display("%0d", c);
    c = (a >= b); $display("%0d", c);
    a = 5; b = 5;
    c = (a == b); $display("%0d", c);
    c = (a <= b); $display("%0d", c);
    c = (a >= b); $display("%0d", c);
  end
endmodule
