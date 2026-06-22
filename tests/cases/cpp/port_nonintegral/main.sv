// A non-integral input port (here a string) drives the child cell like an
// integral one (LRM 23.3.3).
module Child(input string label, output int outlen);
  always_comb outlen = label.len();
endmodule

module Top;
  string s;
  int len;
  Child u(.label(s), .outlen(len));

  initial begin
    s = "hi";
    #5;
    s = "hello";
  end

  final $display("len=%0d", len);
endmodule
