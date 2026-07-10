module Top;
  import "DPI-C" function void get_int(output int x);
  import "DPI-C" function void get_real(output real r);
  import "DPI-C" function void get_str(output string s);
  import "DPI-C" function void bump_int(inout int x);
  import "DPI-C" function void scale_real(inout real r);
  import "DPI-C" function void suffix_str(inout string s);
  import "DPI-C" function int divmod(input int a, input int b, output int rem);
  int i;
  real r;
  string s;
  int q;
  int rm;
  initial begin
    get_int(i);
    get_real(r);
    get_str(s);
    $display("out i=%0d r=%0.1f s=%s", i, r, s);
    i = 10;
    r = 1.5;
    s = "ab";
    bump_int(i);
    scale_real(r);
    suffix_str(s);
    $display("inout i=%0d r=%0.1f s=%s", i, r, s);
    q = divmod(17, 5, rm);
    $display("q=%0d rem=%0d", q, rm);
  end
endmodule
