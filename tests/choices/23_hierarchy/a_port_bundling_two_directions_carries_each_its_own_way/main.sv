// A non-ANSI port may connect a concatenation of internal names, each of
// which is declared a port of its own direction (LRM 23.2.2.1), and the
// standard does not say how a connection is made where those directions
// differ. Lyra carries each bundled name its own way, by bit position, the
// first name taking the most significant bits: an input reads its bits of the
// connection and an output drives its bits. This case holds Lyra to that.
module Split (.io({driven, sensed}));
  output var logic [3:0] driven;
  input var logic [3:0] sensed;

  always_comb driven = sensed + 4'd1;
endmodule

module Top;
  logic [7:0] mixed;
  Split s (.io(mixed));

  initial begin
    mixed[3:0] = 4'h6;
    #1;
  end

  final begin
    if (s.sensed !== 4'h6) $fatal(1, "s.sensed was %0h, expected 6", s.sensed);
    if (mixed[7:4] !== 4'h7)
      $fatal(1, "mixed[7:4] was %0h, expected 7", mixed[7:4]);
    $display("All checks passed");
  end
endmodule
