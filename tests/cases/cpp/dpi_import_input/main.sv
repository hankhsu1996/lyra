module Top;
  import "DPI-C" function int add_one(input int x);
  import "DPI-C" function int mul(input int a, input int b);
  import "DPI-C" function real scale(input real r, input int k);
  import "DPI-C" function real negate(input real r);
  import "DPI-C" function int slen(input string s);
  int r;
  real a;
  string s;
  int n;
  initial begin
    r = add_one(41);
    $display("add_one=%0d", r);
    r = mul(6, 7);
    $display("mul=%0d", r);
    a = scale(1.5, 4);
    $display("scale=%0.1f", a);
    a = negate(2.5);
    $display("negate=%0.1f", a);
    s = "hello world";
    n = slen(s);
    $display("var=%0d", n);
    n = slen("hi");
    $display("lit=%0d", n);
  end
endmodule
