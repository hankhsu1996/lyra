// A port whose expression is a part of a net, placed by a parameter the
// instance is given (LRM 23.2.2.1, 11.5.1). Each instance drives the part its
// own value places, whether the value comes from an instantiation or a loop's
// index, and leaves the rest of the net undriven (LRM 6.6.1).
module Slice #(parameter int S = 0) (input .i(w[S*8+:8]), output logic [31:0] q);
  wire [31:0] w;
  assign q = w;
endmodule

module Top;
  logic [31:0] q[4];
  logic [31:0] q0, q2;

  for (genvar b = 0; b < 4; b++) begin : g
    Slice #(.S(b)) s (.i(8'h5a), .q(q[b]));
  end

  Slice #(.S(0)) s0 (.i(8'h11), .q(q0));
  Slice #(.S(2)) s2 (.i(8'h33), .q(q2));

  initial begin
    #1;
    if (q[0] !== 32'hzzzzzz5a || q[1] !== 32'hzzzz5azz || q[2] !== 32'hzz5azzzz ||
        q[3] !== 32'h5azzzzzz)
      $fatal(1, "blocks drove %h %h %h %h", q[0], q[1], q[2], q[3]);
    if (q0 !== 32'hzzzzzz11 || q2 !== 32'hzz33zzzz) $fatal(1, "instances drove %h and %h", q0, q2);
    $display("All checks passed");
  end
endmodule
