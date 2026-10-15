// A conditional operator chooses between two indexed part-selects whose
// position a loop index, or a parameter, gives. Its result has the width of
// its operands (LRM 11.6.1) whatever bits they were taken from, so every block
// and every instance writes the byte its own position names, from whichever
// operand its condition chooses (LRM 11.4.11, 11.5.1). An assignment to a part
// so placed, delayed within the assignment, holds the value as wide as the
// part until it writes it there (LRM 9.4.5).
module Merge #(parameter int B = 0) (
    input logic [31:0] d,
    input logic [31:0] q,
    input logic c,
    output logic [7:0] w,
    output logic [15:0] x
);
  assign w = c ? d[B*8+:8] : q[B*8+:8];
  initial x = '0;
  always @(d) x[B*8+:8] = #1 d[7:0];
endmodule

module Top;
  logic [31:0] d, q, w;
  logic [3:0] be;
  logic c;
  logic [7:0] m0, m1;
  logic [15:0] x0, x1;

  for (genvar b = 0; b < 4; b++) begin : g
    assign w[b*8+:8] = be[b] ? d[b*8+:8] : q[b*8+:8];
  end

  Merge #(.B(0)) u0 (.d(d), .q(q), .c(c), .w(m0), .x(x0));
  Merge #(.B(1)) u1 (.d(d), .q(q), .c(c), .w(m1), .x(x1));

  initial begin
    #1;
    d = 32'h44332211;
    q = 32'hddccbbaa;
    be = 4'b0101;
    c = 1'b1;
    #2;
    if (w !== 32'hdd33bb11) $fatal(1, "blocks wrote %h", w);
    if (m0 !== 8'h11 || m1 !== 8'h22) $fatal(1, "instances wrote %h and %h", m0, m1);
    if (x0 !== 16'h0011 || x1 !== 16'h1100)
      $fatal(1, "delayed writes left %h and %h", x0, x1);

    be = 4'b1010;
    c = 1'b0;
    #2;
    if (w !== 32'h44cc22aa) $fatal(1, "after a change blocks wrote %h", w);
    if (m0 !== 8'haa || m1 !== 8'hbb)
      $fatal(1, "after a change instances wrote %h and %h", m0, m1);
    $display("All checks passed");
  end
endmodule
