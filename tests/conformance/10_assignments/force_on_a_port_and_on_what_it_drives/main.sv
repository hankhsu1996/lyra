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
// when that process is ended. A port is forced like any variable: as one
// member of a concatenation, whatever its width, and whatever its type. A
// `ref` port is no value of its own but a name of the variable it is
// connected to (LRM 23.3.3.2), so a force on it, from inside or by a
// hierarchical name, and through any number of `ref` ports, is a force on
// that variable, which every reader of the variable sees. A `ref` port
// connected to an input port names that port, a value of its own, so it shows
// what a force on the input gives it, and whoever waits on it is told; a force
// through it, or through a `ref` port connected to it in turn, leaves the
// input's source alone, however late in the hierarchy the input is bound.
//
// The input ports are declared `var`: one with no port kind is a net (LRM
// 23.2.2.3), which no real or 2-state type may be (LRM 6.7.1), and whose
// value with nothing connected is z rather than a variable's default.
module RefOfInput (
    ref logic [7:0] x
);
endmodule

module InputHandedToRef (
    input var logic [7:0] in
);
  RefOfInput leaf (.x(in));
endmodule

module RefOfRef (
    ref logic [7:0] x
);
  RefOfInput inner (.x(x));
endmodule

module InputLeaf (
    input var logic [7:0] in
);
endmodule

module RefWide (
    ref logic [99:0] w
);
endmodule

module RefLeaf (
    ref int i,
    ref real r
);
endmodule

module RefMid (
    ref int i,
    ref real r
);
  RefLeaf leaf (.i(i), .r(r));
  initial begin
    #1;
    force i = 7;
    force r = 2.5;
  end
endmodule

module Leaf (
    input var logic [7:0] in
);
endmodule

module Mid (
    input var logic [7:0] in
);
  Leaf leaf (.in(in));
endmodule

module WideLeaf (
    input var logic [99:0] in
);
endmodule

module RealLeaf (
    input var real in
);
endmodule

module Top;
  logic [7:0] src;
  logic [7:0] own;
  logic [99:0] wide_src;
  real real_src;
  WideLeaf wide_leaf (.in(wide_src));
  RealLeaf real_leaf (.in(real_src));
  Mid mid (.in(src));
  Mid open (.in());
  int shared = 1;
  real shared_real = 1.5;
  RefMid ref_mid (.i(shared), .r(shared_real));
  logic [7:0] input_src = 8'd1;
  InputHandedToRef handed (.in(input_src));
  logic [7:0] chain_src = 8'd1;
  RefOfRef chained (.x(late_leaf.in));
  InputLeaf late_leaf (.in(chain_src));
  int ref_wakes = 0;
  int ref_wakes_before;
  always @(handed.leaf.x) ref_wakes++;
  logic [99:0] wide_shared = {4'hA, 96'd1};
  RefWide ref_wide (.w(wide_shared));

  initial begin
    #1;
    ref_wakes_before = ref_wakes;
    force handed.in = 8'd7;
    #1;
    if (handed.leaf.x !== 8'd7)
      $fatal(1, "a ref port connected to a forced input showed %0d, expected 7",
             handed.leaf.x);
    release handed.in;
    #1;
    force handed.leaf.x = 8'd9;
    #1;
    if (input_src !== 8'd1 || handed.in !== 8'd9 || handed.leaf.x !== 8'd9)
      $fatal(1, "a force through a ref port to an input left %0d at the source and %0d at the input",
             input_src, handed.in);
    input_src = 8'd2;
    release handed.leaf.x;
    #1;
    if (handed.leaf.x !== 8'd2)
      $fatal(1, "a ref port connected to a released input showed %0d", handed.leaf.x);
    if (ref_wakes - ref_wakes_before != 4)
      $fatal(1, "a wait through a ref port following an input woke %0d times, expected 4",
             ref_wakes - ref_wakes_before);
    force chained.inner.x = 8'd12;
    #1;
    if (chain_src !== 8'd1 || late_leaf.in !== 8'd12 || chained.x !== 8'd12)
      $fatal(1, "a force through ref ports to an input bound later left %0d %0d %0d",
             chain_src, late_leaf.in, chained.x);
    release chained.inner.x;
    #1;
    if (late_leaf.in !== 8'd1)
      $fatal(1, "an input bound later showed %0d after its release", late_leaf.in);
    force ref_wide.w = {4'hB, 96'd2};
    #1;
    if (wide_shared !== {4'hB, 96'd2})
      $fatal(1, "a wide variable forced through a ref port was %h", wide_shared);
    release ref_wide.w;
    wide_shared = {4'hC, 96'd3};
    if (ref_wide.w !== {4'hC, 96'd3})
      $fatal(1, "a wide ref port after its release showed %h", ref_wide.w);
  end

  initial begin
    #2;
    shared = 3;
    shared_real = 3.5;
    if (shared !== 7 || shared_real != 2.5)
      $fatal(1, "a variable forced through a ref port was %0d %f", shared,
             shared_real);
    release ref_mid.leaf.i;
    release ref_mid.leaf.r;
    if (shared !== 7 || shared_real != 2.5)
      $fatal(1, "a variable released through a ref port was %0d %f", shared,
             shared_real);
    shared = 4;
    if (ref_mid.i !== 4)
      $fatal(1, "a ref port after its release showed %0d, expected 4",
             ref_mid.i);
    force ref_mid.leaf.i = 9;
    #1;
    if (shared !== 9)
      $fatal(1, "a force by the name of a ref port left the variable at %0d",
             shared);
    release shared;
    shared = 5;
    if (ref_mid.leaf.i !== 5)
      $fatal(1, "a ref port after its variable's release showed %0d",
             ref_mid.leaf.i);

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

    force {mid.in, own} = 16'h0A0B;
    #1;
    if (mid.in !== 8'h0A || own !== 8'h0B)
      $fatal(1, "a forced concatenation left a port at %h and a variable at %h",
             mid.in, own);
    if (mid.leaf.in !== 8'h0A)
      $fatal(1, "the port below one forced in a concatenation was %h",
             mid.leaf.in);
    release {mid.in, own};
    #1;
    if (mid.in !== 8'd5 || own !== 8'h0B)
      $fatal(1, "a released concatenation left a port at %0d and a variable at %h",
             mid.in, own);

    wide_src = {4'hC, 96'd1};
    #1;
    force wide_leaf.in = 100'd7;
    #1;
    if (wide_leaf.in !== 100'd7)
      $fatal(1, "a forced wide port was %h, expected 7", wide_leaf.in);
    if (wide_src !== {4'hC, 96'd1})
      $fatal(1, "forcing a wide port moved its source to %h", wide_src);
    wide_src = {4'hD, 96'd2};
    release wide_leaf.in;
    #1;
    if (wide_leaf.in !== {4'hD, 96'd2})
      $fatal(1, "a released wide port was %h", wide_leaf.in);

    real_src = 1.5;
    #1;
    force real_leaf.in = 2.5;
    #1;
    if (real_leaf.in != 2.5)
      $fatal(1, "a forced real port was %f, expected 2.5", real_leaf.in);
    if (real_src != 1.5)
      $fatal(1, "forcing a real port moved its source to %f", real_src);
    real_src = 3.5;
    release real_leaf.in;
    #1;
    if (real_leaf.in != 3.5)
      $fatal(1, "a released real port was %f, expected 3.5", real_leaf.in);

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
