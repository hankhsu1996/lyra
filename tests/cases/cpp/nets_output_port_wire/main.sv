// A net-typed output port (LRM 23.3.3): a child drives its `output wire`, and
// the connection carries that value to the parent across the boundary. The
// parent side is read uniformly and driven per its own kind -- a variable takes
// the value directly, a parent net takes it through a driver. A chain of output
// nets (`Leaf -> Mid -> Top`) converges because each net is sensitive to the
// child net it reads. The single-driver case here reuses the resolution model;
// driven values are 2-state so this runs in both state modes.
module Source(output wire [7:0] o);
  assign o = 8'd33;
endmodule

module Mid(output wire [7:0] o);
  Source inner(.o(o));
endmodule

module Top;
  logic [7:0] seen_var;
  wire  [7:0] from_net;
  wire  [7:0] from_chain;

  Source to_var(.o(seen_var));
  Source to_net(.o(from_net));
  Mid    chain(.o(from_chain));

  initial begin
    #1;
    $display("var=%0d net=%0d chain=%0d", seen_var, from_net, from_chain);
  end
endmodule
