module Top;
  initial begin
    int a;
    int b;
    a = 0;
    b = 0;
    if (1) a = 7;
    if (0) b = 9;
    $display("a=%0d", a);
    $display("b=%0d", b);
  end
endmodule
