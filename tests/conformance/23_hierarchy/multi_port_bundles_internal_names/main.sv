// A non-ANSI port list may name a port explicitly and connect it to a
// concatenation of internal names, so one external port carries several
// internal ones (LRM 23.2.2.1). The external name is what a connection uses;
// the bundled names belong to the module. The connection is split across them
// by bit position, the first name in the concatenation receiving the most
// significant bits (LRM 23.2.2.1).
//
// The bundled names need not share a direction, so the port's own direction is
// the most restrictive of theirs and says nothing about which way any one of
// them carries data. A connection therefore has to read each bundled name's
// own direction rather than the port's.
module Bundle (.ab({a, b}));
  input var logic [3:0] a;
  input var logic [3:0] b;
  int sum;

  always_comb sum = a + b;
endmodule

module Split (.io({driven, sensed}));
  output var logic [3:0] driven;
  input var logic [3:0] sensed;

  always_comb driven = sensed + 4'd1;
endmodule

module Top;
  logic [7:0] bundled;
  Bundle u (.ab(bundled));

  logic [7:0] mixed;
  Split s (.io(mixed));

  initial begin
    bundled = 8'h35;
    mixed[3:0] = 4'h6;
    #1;
  end

  final begin
    if (u.a !== 4'h3) $fatal(1, "u.a was %0h, expected 3", u.a);
    if (u.b !== 4'h5) $fatal(1, "u.b was %0h, expected 5", u.b);
    if (u.sum !== 8) $fatal(1, "u.sum was %0d, expected 8", u.sum);
    if (s.sensed !== 4'h6) $fatal(1, "s.sensed was %0h, expected 6", s.sensed);
    if (mixed[7:4] !== 4'h7)
      $fatal(1, "mixed[7:4] was %0h, expected 7", mixed[7:4]);
    $display("All checks passed");
  end
endmodule
