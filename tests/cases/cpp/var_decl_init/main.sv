module Top;
  initial begin
    int x = 7;
    int y = 1 + 2;
    int z = x + y;
    int neg = -3;
    int compound = neg - 4;
    $display("x=%0d", x);
    $display("y=%0d", y);
    $display("z=%0d", z);
    $display("neg=%0d", neg);
    $display("compound=%0d", compound);
  end
endmodule
