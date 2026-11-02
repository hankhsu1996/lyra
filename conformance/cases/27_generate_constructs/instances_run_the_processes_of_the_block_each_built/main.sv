// Instances of one module given different parameter values build different
// generate blocks (LRM 27.5), and each instance runs the processes of the block
// it built: a nonblocking assignment writes what its own left side names (LRM
// 10.4.2), and a process waits on the event control it was written with (LRM
// 9.4.2). The two instances here differ only in which block each selected, so a
// process of one block running for the other instance writes the other bit, or
// waits on the other control.
//
// The two blocks' first processes differ only in what they write; their
// second processes also differ in the event control each waits on.
//
// A structure the module declares starts each instance's variable at what its
// member initializers give (LRM 7.2.2), whichever block the instance built.
module Stage #(
    parameter int R = 0
) (
    input logic clk,
    input logic d
);
  typedef struct {
    int x = 5;
    int y = 9;
  } pair_t;

  pair_t p;
  logic [2:1] s = '0;
  logic [2:1] t = '0;

  if (R == 1) begin : low
    always_ff @(posedge clk) s[1] <= d;
    always_ff @(posedge clk or negedge d) begin
      t[1] <= d;
      t[2] <= t[1];
    end
  end else begin : high
    always_ff @(posedge clk) s[2] <= d;
    always_ff @(posedge clk) begin
      t[1] <= d;
      t[2] <= t[1];
    end
  end
endmodule

module Top;
  logic clk = 0;
  logic d = 1;

  Stage #(.R(0)) u0 (.clk, .d);
  Stage #(.R(1)) u1 (.clk, .d);

  always #5 clk = ~clk;

  initial begin
    #22;
    if (u0.s !== 2'b10) $fatal(1, "u0.s is %b, expected 10", u0.s);
    if (u1.s !== 2'b01) $fatal(1, "u1.s is %b, expected 01", u1.s);
    if (u0.t !== 2'b11) $fatal(1, "u0.t is %b, expected 11", u0.t);
    if (u1.t !== 2'b11) $fatal(1, "u1.t is %b, expected 11", u1.t);
    if (u0.p.x !== 5 || u0.p.y !== 9)
      $fatal(1, "u0.p is %0d %0d, expected 5 9", u0.p.x, u0.p.y);
    if (u1.p.x !== 5 || u1.p.y !== 9)
      $fatal(1, "u1.p is %0d %0d, expected 5 9", u1.p.x, u1.p.y);
    $display("All checks passed");
    $finish;
  end
endmodule
