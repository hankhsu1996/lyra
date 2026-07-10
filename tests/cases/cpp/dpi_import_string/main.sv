module Top;
  import "DPI-C" function int slen(input string s);
  string s;
  int n;
  initial begin
    s = "hello world";
    n = slen(s);
    $display("var=%0d", n);
    n = slen("hi");
    $display("lit=%0d", n);
  end
endmodule
