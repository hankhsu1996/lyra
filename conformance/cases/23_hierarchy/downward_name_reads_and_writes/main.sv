// A hierarchical name reaches an object inside an instance below the referring
// scope by naming the instances on the way down, and the name may be read in
// an expression or written in an assignment exactly as a local reference to
// that object would be (LRM 23.6). The path descends through as many instances
// as separate it from the object, and what it ends on may be a net as readily
// as a variable.
module Leaf;
  int deep_var;

  initial deep_var = 42;
endmodule

module Mid;
  Leaf leaf();
endmodule

module Child;
  int own;
  wire [7:0] driven;

  assign driven = 8'd5;
  initial own = 7;
endmodule

module Top;
  Child child();
  Mid mid();

  int read_var;
  logic [7:0] read_net;
  int read_deep;

  initial begin
    #1;
    read_var = child.own;
    read_net = child.driven;
    read_deep = mid.leaf.deep_var;
    child.own = 99;
    mid.leaf.deep_var = 88;
  end

  final begin
    if (read_var !== 7) $fatal(1, "read_var was %0d, expected 7", read_var);
    if (read_net !== 8'd5)
      $fatal(1, "read_net was %0d, expected 5", read_net);
    if (read_deep !== 42)
      $fatal(1, "read_deep was %0d, expected 42", read_deep);
    if (child.own !== 99)
      $fatal(1, "child.own was %0d, expected 99", child.own);
    if (mid.leaf.deep_var !== 88)
      $fatal(1, "mid.leaf.deep_var was %0d, expected 88", mid.leaf.deep_var);
    $display("All checks passed");
  end
endmodule
