// A continuous assignment written inside a generate block is the same
// construct as one written directly in the module body, and its left-hand side
// may name a data object declared in the scope that encloses the block
// (LRM 10.3.2). The target may be a variable, which takes the driven value
// directly, or a net, whose value is then the resolution of the driver the
// assignment installs (LRM 6.6.1).
module Top;
  logic [7:0] source;
  int to_variable;
  wire [7:0] to_net;

  logic [7:0] seen_net;
  int seen_variable;

  if (1) begin : g
    assign to_variable = source + 8'd1;
    assign to_net = source + 8'd2;
  end

  initial begin
    source = 8'd41;
    #1;
    seen_variable = to_variable;
    seen_net = to_net;
  end

  final begin
    if (seen_variable !== 42)
      $fatal(1, "seen_variable was %0d, expected 42", seen_variable);
    if (seen_net !== 8'd43)
      $fatal(1, "seen_net was %0d, expected 43", seen_net);
    $display("All checks passed");
  end
endmodule
