// Two instances of one module with different value-parameter bindings must
// each behave according to their own binding (LRM 6.20.2, 23.10). They are
// distinct specializations and must not collapse onto one compiled artifact.
module Reg #(parameter int INIT = 0) (output int q);
  assign q = INIT;
endmodule

module Test;
  int qa;
  int qb;
  Reg #(.INIT(3)) a (.q(qa));
  Reg #(.INIT(7)) b (.q(qb));
  final $display("qa=%0d qb=%0d", qa, qb);
endmodule
