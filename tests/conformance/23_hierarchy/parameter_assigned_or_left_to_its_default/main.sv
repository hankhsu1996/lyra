// A parameter an instantiation leaves unassigned takes the default its
// declaration states, and a default may read the parameters declared before
// it (LRM 6.20.1). Instances of one module that assign a parameter and
// instances that leave it to its default each hold their own value, whether
// the earlier parameters it reads were assigned or defaulted themselves, and
// whether the value was given by the instantiation or by a defparam (LRM
// 23.10). A default no instance is left to need not be valid against the
// values the other parameters were given.
module Leaf #(
    parameter int W = 8,
    parameter int D = W * 2,
    parameter int E = D + 1,
    parameter string NAME = "leaf",
    parameter int NEVER = W(1)
) (
    output int w,
    output int d,
    output int e,
    output int name_len
);
  initial begin
    w = W;
    d = D;
    e = E;
    name_len = NAME.len();
  end
endmodule

module Top;
  int w[6];
  int d[6];
  int e[6];
  int name_len[6];

  Leaf #(.W(4), .D(100), .E(5), .NAME("ab"), .NEVER(0)) all_assigned (
      w[0], d[0], e[0], name_len[0]
  );
  Leaf #(.W(4), .NEVER(0)) earlier_assigned (w[1], d[1], e[1], name_len[1]);
  Leaf #(.D(3), .NEVER(0)) later_assigned (w[2], d[2], e[2], name_len[2]);
  Leaf #(.NEVER(0)) none_assigned (w[3], d[3], e[3], name_len[3]);
  Leaf #(.W(6), .NEVER(0)) by_defparam (w[4], d[4], e[4], name_len[4]);
  Leaf #(.W(7), .NEVER(0)) in_order (w[5], d[5], e[5], name_len[5]);
  defparam by_defparam.D = 50;

  final begin
    if (w[0] !== 4 || d[0] !== 100 || e[0] !== 5 || name_len[0] !== 2)
      $fatal(1, "all_assigned held %0d %0d %0d %0d", w[0], d[0], e[0],
             name_len[0]);
    if (w[1] !== 4 || d[1] !== 8 || e[1] !== 9 || name_len[1] !== 4)
      $fatal(1, "earlier_assigned held %0d %0d %0d %0d", w[1], d[1], e[1],
             name_len[1]);
    if (w[2] !== 8 || d[2] !== 3 || e[2] !== 4)
      $fatal(1, "later_assigned held %0d %0d %0d", w[2], d[2], e[2]);
    if (w[3] !== 8 || d[3] !== 16 || e[3] !== 17)
      $fatal(1, "none_assigned held %0d %0d %0d", w[3], d[3], e[3]);
    if (w[4] !== 6 || d[4] !== 50 || e[4] !== 51)
      $fatal(1, "by_defparam held %0d %0d %0d", w[4], d[4], e[4]);
    if (w[5] !== 7 || d[5] !== 14 || e[5] !== 15)
      $fatal(1, "in_order held %0d %0d %0d", w[5], d[5], e[5]);
    $display("All checks passed");
  end
endmodule
