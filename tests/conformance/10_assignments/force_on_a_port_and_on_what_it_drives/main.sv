// A force on a variable overrides whatever drives it until a release (LRM
// 10.6.2), and what the variable drives follows the forced value as it would
// any other. A port is driven by its connection (LRM 23.3.3), so a port handed
// on below a forced one shows the forced value; a force of its own on the port
// below outranks that until it is released, when it shows what drives it
// again, which is then the forced value above. A second force replaces the
// first, a force whose expression reads a variable follows that variable, and
// a force on a port left unconnected gives way on release to the default
// initial value the port holds (LRM 23.3.3.2). A force is no part of the
// process that made it, so it stays in effect, still following its expression,
// when that process is ended.
module Leaf (
    input logic [7:0] in
);
endmodule

module Mid (
    input logic [7:0] in
);
  Leaf leaf (.in(in));
endmodule

module Top;
  logic [7:0] src;
  Mid mid (.in(src));
  Mid open (.in());

  initial begin
    src = 8'd1;
    #1;
    force mid.in = 8'd10;
    #1;
    if (mid.in !== 8'd10) $fatal(1, "a forced port was %0d, expected 10", mid.in);
    if (mid.leaf.in !== 8'd10)
      $fatal(1, "the port below a forced one was %0d, expected 10", mid.leaf.in);

    force mid.leaf.in = 8'd20;
    #1;
    if (mid.leaf.in !== 8'd20)
      $fatal(1, "a port forced below a forced one was %0d, expected 20",
             mid.leaf.in);
    if (mid.in !== 8'd10)
      $fatal(1, "a force below moved the port above to %0d", mid.in);

    force mid.in = 8'd30;
    #1;
    if (mid.in !== 8'd30)
      $fatal(1, "a second force left the port at %0d, expected 30", mid.in);
    if (mid.leaf.in !== 8'd20)
      $fatal(1, "a force above outranked the one below, port was %0d",
             mid.leaf.in);

    release mid.leaf.in;
    #1;
    if (mid.leaf.in !== 8'd30)
      $fatal(1, "a port released below a forced one was %0d, expected 30",
             mid.leaf.in);

    release mid.in;
    #1;
    if (mid.in !== 8'd1) $fatal(1, "a released port was %0d, expected 1", mid.in);
    if (mid.leaf.in !== 8'd1)
      $fatal(1, "the port below a released one was %0d, expected 1", mid.leaf.in);
    if (src !== 8'd1) $fatal(1, "forcing its ports moved the source to %0d", src);

    force mid.in = src + 8'd5;
    #1;
    if (mid.in !== 8'd6) $fatal(1, "a forced expression was %0d, expected 6", mid.in);
    src = 8'd2;
    #1;
    if (mid.in !== 8'd7)
      $fatal(1, "a forced expression did not follow its operand, was %0d",
             mid.in);
    release mid.in;
    #1;
    if (mid.in !== 8'd2) $fatal(1, "a released port was %0d, expected 2", mid.in);

    force open.in = 8'd4;
    #1;
    if (open.in !== 8'd4)
      $fatal(1, "a forced unconnected port was %0d, expected 4", open.in);
    if (open.leaf.in !== 8'd4)
      $fatal(1, "the port below a forced unconnected one was %0d, expected 4",
             open.leaf.in);
    release open.in;
    #1;
    if (open.in !== 8'bx)
      $fatal(1, "a released unconnected port was %b, expected all x", open.in);

    force mid.leaf.in = 8'd3;
    force mid.in = 8'd9;
    release mid.in;
    src = 8'd5;
    #1;
    if (mid.in !== 8'd5)
      $fatal(1, "a port released above a forced one was %0d, expected 5", mid.in);
    if (mid.leaf.in !== 8'd3)
      $fatal(1, "a release above ended the force below, port was %0d",
             mid.leaf.in);
    release mid.leaf.in;
    #1;
    if (mid.leaf.in !== 8'd5)
      $fatal(1, "the port released last was %0d, expected 5", mid.leaf.in);

    fork
      begin
        force mid.in = src + 8'd100;
        #10;
      end
    join_none
    #1 disable fork;
    #1;
    if (mid.in !== 8'd105)
      $fatal(1, "ending the forcing process left the port at %0d, expected 105",
             mid.in);
    src = 8'd6;
    #1;
    if (mid.leaf.in !== 8'd106)
      $fatal(1, "a force outliving its process stopped following, was %0d",
             mid.leaf.in);
    release mid.in;
    #1;
    if (mid.in !== 8'd6) $fatal(1, "a released port was %0d, expected 6", mid.in);

    $display("All checks passed");
  end
endmodule
