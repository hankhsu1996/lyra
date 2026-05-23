module Top;
  initial begin
    int a = 5;
    int b = a + 1;
    int c = a + b;
    $display("a=%0d", a);
    $display("b=%0d", b);
    $display("c=%0d", c);
  end
endmodule
