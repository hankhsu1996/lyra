// An always_comb procedure reads an element past its array's end only in a
// branch that its own loop index, or its own parameter, never takes. The
// procedure is still sensitive to every variable it reads (LRM 9.2.2.2.1
// excepts no branch a constant decides), and the element past the end reads
// the same value whatever the array holds (LRM 7.4.5), so each block and each
// instance follows the element its taken branch reads and computes its own
// answer from it.
module Shift #(parameter int P = 0) (input logic [7:0] q[2], output logic [7:0] y);
  always_comb begin
    if (P != 1) y = q[P+1];
    else y = q[0];
  end
endmodule

module Top;
  logic [7:0] q[2];
  logic [7:0] y[2];
  logic [7:0] z0, z1;

  for (genvar i = 0; i < 2; i++) begin : g
    always_comb begin
      if (i != 1) y[i] = q[i+1];
      else y[i] = q[0];
    end
  end

  Shift #(.P(0)) s0 (.q(q), .y(z0));
  Shift #(.P(1)) s1 (.q(q), .y(z1));

  initial begin
    q[0] = 8'h11;
    q[1] = 8'h22;
    #1;
    if (y[0] !== 8'h22 || y[1] !== 8'h11)
      $fatal(1, "blocks read %h and %h", y[0], y[1]);
    if (z0 !== 8'h22 || z1 !== 8'h11)
      $fatal(1, "instances read %h and %h", z0, z1);

    q[0] = 8'h33;
    q[1] = 8'h44;
    #1;
    if (y[0] !== 8'h44 || y[1] !== 8'h33)
      $fatal(1, "after a change blocks read %h and %h", y[0], y[1]);
    if (z0 !== 8'h44 || z1 !== 8'h33)
      $fatal(1, "after a change instances read %h and %h", z0, z1);
    $display("All checks passed");
  end
endmodule
