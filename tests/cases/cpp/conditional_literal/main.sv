module Top;
  initial begin
    int a;
    if (1) a = 42;
    else   a = 0;
    $display("a=%0d", a);
  end
endmodule
