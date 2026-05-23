module Top;
  int a, b, c;
  initial begin
    a = 5;
    b = +a; $display("%0d", b);
    b = -a; $display("%0d", b);
    b = ~a; $display("%0d", b);
    b = !a; $display("%0d", b);
    a = 0;
    b = !a; $display("%0d", b);
    c = -42; $display("%0d", c);
    a = -7; $display("%0d", a);
  end
endmodule
