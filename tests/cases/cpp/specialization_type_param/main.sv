// A type parameter changes the compiled type, so distinct type bindings are
// distinct specializations (LRM 6.20.3, 23.10) and must not collapse onto one
// artifact. The cast truncates under byte (8-bit) but not under int.
module Box #(parameter type T = int) (output int q);
  assign q = T'(300);
endmodule

module Test;
  int qa;
  int qb;
  Box #(.T(byte)) a (.q(qa));
  Box #(.T(int)) b (.q(qb));
  final $display("qa=%0d qb=%0d", qa, qb);
endmodule
