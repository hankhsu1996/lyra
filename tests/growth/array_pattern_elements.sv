// An unpacked array of N elements, assigned from a pattern naming each.
// @each-adds: bytes,
// @each-adds: mir.constants 1, lir.constants 1
module Top #(parameter int N = 4);
  int a[N];

  initial a = '{[[{i}|, ]]};
endmodule
