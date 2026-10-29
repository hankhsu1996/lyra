// A non-ANSI port list may name a port explicitly and connect it to a
// concatenation of internal names, so one external port carries several
// internal ones (LRM 23.2.2.1). The external name is what a connection uses;
// the bundled names belong to the module. The connection is split across them
// by bit position, the first name in the concatenation receiving the most
// significant bits (LRM 23.2.2.1).
module Bundle (.ab({a, b}));
  input var logic [3:0] a;
  input var logic [3:0] b;
  int sum;

  always_comb sum = a + b;
endmodule

module Top;
  logic [7:0] bundled;
  Bundle u (.ab(bundled));

  initial begin
    bundled = 8'h35;
    #1;
  end

  final begin
    if (u.a !== 4'h3) $fatal(1, "u.a was %0h, expected 3", u.a);
    if (u.b !== 4'h5) $fatal(1, "u.b was %0h, expected 5", u.b);
    if (u.sum !== 8) $fatal(1, "u.sum was %0d, expected 8", u.sum);
    $display("All checks passed");
  end
endmodule
