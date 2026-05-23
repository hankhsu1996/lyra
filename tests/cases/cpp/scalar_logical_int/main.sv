module Top;
  int a, b, c;
  initial begin
    a = 5; b = 0;
    c = a && b; $display("%0d", c);
    c = a || b; $display("%0d", c);
    a = 5; b = 7;
    c = a && b; $display("%0d", c);
    a = 0; b = 0;
    c = a || b; $display("%0d", c);
  end
endmodule
