// A concurrent assertion reads every variable at its sampled value (LRM
// 16.5.1), and a constant is no variable: its value is the same at every time,
// so it stands in an assertion, and in a sampled value function's arguments, as
// itself. This reads each kind of constant a design writes -- an enumeration
// name, a member of a structure-typed localparam, a package's localparam, a
// parameter each instance is handed its own value of, and a loop generate's
// index -- and counts what each assertion answered at every tick.
package limits;
  localparam int kTicksBack = 2;
endpackage

module Sub #(parameter int W = 1) (input logic clk);
  int passes = 0;
  int fails = 0;
  int ticks = 0;
  always @(posedge clk) ticks <= ticks + 1;
  // A variable beside the constant keeps the condition one the front end does
  // not fold away.
  assert property (@(posedge clk) W == 3 && ticks >= 0) passes++;
  else fails++;
endmodule

module Top;
  typedef enum {kIdle, kBusy} state_t;
  typedef struct packed {logic [3:0] f;} pair_t;
  localparam pair_t kPair = '{f: 4'd5};
  localparam int kStep = 1;

  logic clk = 0;
  int n = 0;
  int folded_passes = 0;
  int folded_fails = 0;
  int past_passes = 0;
  int past_fails = 0;
  int lane_passes[2] = '{0, 0};
  int lane_fails[2] = '{0, 0};

  always #5 clk = ~clk;
  always @(posedge clk) n <= n + kStep;

  Sub #(.W(3)) given_three (clk);
  Sub #(.W(4)) given_four (clk);

  assert property (@(posedge clk)
      kBusy != kIdle && kPair.f == 5 && limits::kTicksBack == 2)
    folded_passes++;
  else folded_fails++;

  // `n` is k at the k-th tick, counted from zero, so two ticks back it was
  // k - 2, and before there were two ticks it reads its default (LRM 16.9.3).
  assert property (@(posedge clk)
      $past(n, limits::kTicksBack) == (n >= limits::kTicksBack ? n - 2 : 0))
    past_passes++;
  else past_fails++;

  for (genvar i = 0; i < 2; i++) begin : lane
    assert property (@(posedge clk) i == 1 && n >= 0) lane_passes[i]++;
    else lane_fails[i]++;
  end

  initial #100 $finish;

  final begin
    // The clock rises at 5, 15, ..., 95: ten ticks, each one attempt.
    if (folded_passes !== 10 || folded_fails !== 0)
      $fatal(1, "folded constants passed %0d and failed %0d, expected 10 and 0",
             folded_passes, folded_fails);
    if (past_passes !== 10 || past_fails !== 0)
      $fatal(1, "a constant tick count passed %0d and failed %0d, expected 10 and 0",
             past_passes, past_fails);
    if (given_three.passes !== 10 || given_three.fails !== 0)
      $fatal(1, "the instance given 3 passed %0d and failed %0d, expected 10 and 0",
             given_three.passes, given_three.fails);
    if (given_four.passes !== 0 || given_four.fails !== 10)
      $fatal(1, "the instance given 4 passed %0d and failed %0d, expected 0 and 10",
             given_four.passes, given_four.fails);
    if (lane_passes[0] !== 0 || lane_fails[0] !== 10)
      $fatal(1, "lane 0 passed %0d and failed %0d, expected 0 and 10",
             lane_passes[0], lane_fails[0]);
    if (lane_passes[1] !== 10 || lane_fails[1] !== 0)
      $fatal(1, "lane 1 passed %0d and failed %0d, expected 10 and 0",
             lane_passes[1], lane_fails[1]);
    $display("All checks passed");
  end
endmodule
