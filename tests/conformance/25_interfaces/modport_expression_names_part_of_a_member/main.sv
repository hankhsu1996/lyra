// A modport expression gives a port identifier its own meaning inside the
// interface (LRM 25.5.4): the identifier names an element, a part-select, a
// concatenation, an assignment pattern, or a constant built from what the
// interface declared, and a module's access through that identifier reaches
// exactly that. Port identifiers live in each modport's own name space, so one
// name carries a different meaning per modport, and a module written once
// against the name acts on whichever part the modport it was bound through
// named. A port expression is self-determined and is not an assignment-like
// context, and it is optional, so a port may connect to nothing internal at
// all.
interface Nibbles;
  logic [7:0] r;
  const int   one = 1;
  bit         flag;

  modport low(output .Part(r[3:0]), input .Value(one), flag);
  modport high(output .Part(r[7:4]), input .Value(2), flag);
endinterface

module Writer (
    interface i
);
  bit saw_flag = 1'b0;

  initial #1 begin
    i.Part  = i.Value;
    saw_flag = i.flag;
  end
endmodule

module Top;
  Nibbles n ();

  Writer low (.i(n.low));
  Writer high (.i(n.high));

  initial n.flag = 1'b1;

  final begin
    if (n.r !== 8'b0010_0001)
      $fatal(1, "n.r was %b, expected 00100001", n.r);
    // A plain identifier beside the expression ports keeps its own meaning:
    // it names the interface item itself, under either modport.
    if (low.saw_flag !== 1'b1)
      $fatal(1, "low.saw_flag was %b, expected 1", low.saw_flag);
    if (high.saw_flag !== 1'b1)
      $fatal(1, "high.saw_flag was %b, expected 1", high.saw_flag);
    $display("All checks passed");
  end
endmodule
