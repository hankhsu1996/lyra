// A force overrides whatever drives a variable until it is released, and
// releasing a variable that a continuous assignment drives reestablishes that
// assignment and reevaluates it (LRM 10.6.2), so the variable shows what its
// driver holds at the release and not what it held when the force began. A
// port connection is a continuous assignment of source to sink (LRM 23.3.3),
// so the same holds of a port: a force on the sink reaches everything the sink
// drives and leaves the source alone, and while it is in effect a change of
// the source is no change of the sink. A continuous assignment inside a module
// to a variable the module reaches through a ref port (LRM 23.3.3.2) drives
// that variable like any other, so releasing the variable reestablishes it.
module Leaf(input logic [7:0] in);
  int changes;
  always @(in) changes++;
endmodule

module Middle(input logic [7:0] in, output logic [7:0] out);
  logic [7:0] held;
  Leaf leaf(.in(in));
  always_comb held = in;
  assign out = held;
endmodule

module Through(ref logic [7:0] target, input logic [7:0] in);
  assign target = in + 8'd2;
endmodule

module Top;
  logic [7:0] src;
  logic [7:0] copy;
  logic [7:0] sum;
  logic [7:0] res;
  assign copy = src;
  assign sum = src + 8'd1;
  Middle mid(.in(src), .out(res));
  logic [7:0] lent;
  Through through(.target(lent), .in(src));

  initial begin
    src = 8'd7;
    #1;
    if (copy !== 8'd7) $fatal(1, "copy was %0d before any force, expected 7", copy);
    if (sum !== 8'd8) $fatal(1, "sum was %0d before any force, expected 8", sum);
    if (res !== 8'd7) $fatal(1, "res was %0d before any force, expected 7", res);

    force copy = 8'd55;
    force sum = 8'd66;
    force lent = 8'd77;
    #1;
    if (lent !== 8'd77) $fatal(1, "a forced lent was %0d, expected 77", lent);
    if (copy !== 8'd55) $fatal(1, "a forced copy was %0d, expected 55", copy);
    if (sum !== 8'd66) $fatal(1, "a forced sum was %0d, expected 66", sum);
    if (src !== 8'd7) $fatal(1, "forcing a sink moved its source to %0d", src);

    src = 8'd9;
    #1;
    if (copy !== 8'd55) $fatal(1, "a driver outranked a force, copy was %0d", copy);
    if (sum !== 8'd66) $fatal(1, "a driver outranked a force, sum was %0d", sum);

    release copy;
    release sum;
    release lent;
    #1;
    if (lent !== 8'd11)
      $fatal(1, "a variable driven through a ref port was %0d once released",
             lent);
    if (copy !== 8'd9)
      $fatal(1, "a released copy was %0d, expected its driver's 9", copy);
    if (sum !== 8'd10)
      $fatal(1, "a released sum was %0d, expected its driver's 10", sum);

    mid.leaf.changes = 0;
    force mid.in = 8'd100;
    #1;
    if (mid.in !== 8'd100) $fatal(1, "a forced port was %0d, expected 100", mid.in);
    if (src !== 8'd9) $fatal(1, "forcing a port moved its source to %0d", src);
    if (mid.leaf.in !== 8'd100)
      $fatal(1, "a port below a forced one was %0d, expected 100", mid.leaf.in);
    if (res !== 8'd100)
      $fatal(1, "what a forced port drives was %0d, expected 100", res);
    if (mid.leaf.changes !== 1)
      $fatal(1, "forcing a port was %0d changes below it, expected 1",
             mid.leaf.changes);

    src = 8'd11;
    #1;
    if (mid.in !== 8'd100)
      $fatal(1, "a source outranked a force on its port, port was %0d", mid.in);
    if (mid.leaf.changes !== 1)
      $fatal(1, "a source moving under a force was a change below, %0d seen",
             mid.leaf.changes);

    release mid.in;
    #1;
    if (mid.in !== 8'd11)
      $fatal(1, "a released port was %0d, expected its source's 11", mid.in);
    if (mid.leaf.in !== 8'd11)
      $fatal(1, "a port below a released one was %0d, expected 11", mid.leaf.in);
    if (res !== 8'd11)
      $fatal(1, "what a released port drives was %0d, expected 11", res);
    if (mid.leaf.changes !== 2)
      $fatal(1, "releasing a port was %0d changes below it, expected 2",
             mid.leaf.changes);

    force res = 8'd200;
    #1;
    if (res !== 8'd200) $fatal(1, "a forced output actual was %0d, expected 200", res);
    if (mid.out !== 8'd11)
      $fatal(1, "forcing an output's actual moved the port to %0d", mid.out);
    release res;
    #1;
    if (res !== 8'd11)
      $fatal(1, "a released output actual was %0d, expected the port's 11", res);

    $display("All checks passed");
  end
endmodule
