// LRM 11.4.5: when an element comparison is ambiguous due to X / Z, the
// aggregate `==` / `!=` result is 1'bx.
module Top;
  logic [3:0] a [3];
  logic [3:0] b [3];
  logic eq_with_x, ne_with_x;
  initial begin
    a[0] = 4'b1010; b[0] = 4'b1010;
    a[1] = 4'b10x0; b[1] = 4'b1010;
    a[2] = 4'b1111; b[2] = 4'b1111;
    eq_with_x = (a == b);
    ne_with_x = (a != b);
  end
endmodule
