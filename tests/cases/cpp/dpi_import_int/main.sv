module Top;
  import "DPI-C" function int add_one(input int x);
  import "DPI-C" function int mul(input int a, input int b);
  int r;
  initial begin
    r = add_one(41);
    $display("add_one=%0d", r);
    r = mul(6, 7);
    $display("mul=%0d", r);
  end
endmodule
