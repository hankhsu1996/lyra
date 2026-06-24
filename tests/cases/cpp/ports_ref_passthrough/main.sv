// A `ref` port forwarded through an intermediate module to a deeper child (LRM
// 23.3.3.2). `Mid` does not connect `mr` to its own storage; it passes the
// reference straight on to `Leaf`. Every reference on the chain must denote the
// same final cell -- `Top.shared` -- with no reference-to-a-reference: a write
// by `Leaf` lands directly on `Top.shared`. This exercises the resolve phase
// binding references top-down (so the intermediate is bound before it forwards)
// and reference sealing (the forwarded reference flattens to the final cell).
module Leaf(ref int lr);
  initial #1 lr = lr + 50;
endmodule

module Mid(ref int mr);
  Leaf leaf(.lr(mr));
endmodule

module Top;
  int shared = 7;
  Mid mid(.mr(shared));
  final $display("shared=%0d", shared);
endmodule
