// A parameter declared in a named block, a task or a function is redefined
// only by a defparam (LRM 23.10.2); only a generate block's, a package's, a
// compilation-unit scope's and a class's parameter is a localparam (LRM
// 6.20.1). The defparam reaches the one instance it names.
module Leaf;
  int got = 0;
  int from_function = 0;

  function automatic int Scaled();
    parameter int S = 2;
    return S * 10;
  endfunction

  initial begin : blk
    parameter int P = 1;
    got = P;
    from_function = Scaled();
  end
endmodule

module Top;
  Leaf a (), b ();
  defparam b.blk.P = 9;
  defparam b.Scaled.S = 3;

  final begin
    if (a.got !== 1) $fatal(1, "a.blk.P was %0d", a.got);
    if (b.got !== 9) $fatal(1, "b.blk.P was %0d", b.got);
    if (a.from_function !== 20) $fatal(1, "a.Scaled gave %0d", a.from_function);
    if (b.from_function !== 30) $fatal(1, "b.Scaled gave %0d", b.from_function);
    $display("All checks passed");
  end
endmodule
