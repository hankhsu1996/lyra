// @args: --disable-assertions
//
// A statement label creates a named block around the statement it labels, so
// the label names a scope rather than the assertion itself (LRM 16.3). An
// assertion states what the design is required to do and never drives it, so
// the logic beside one computes the same values whether or not the assertion
// is checked -- at module level, inside a generate block, and inside a
// procedure alike.
module Top;
  localparam bit Checked = 1'b1;

  logic clk;
  logic gate;
  int ticks;

  initial begin
    ticks = 0;
    clk = 1'b0;
    repeat (8) #5 clk = ~clk;
  end

  always_ff @(posedge clk) ticks <= ticks + 1;

  always_comb begin
    gate = (ticks >= 0);
    a_immediate: assert (gate);
    a_deferred: assert #0 (gate);
  end

  a_module_level: assert property (@(posedge clk) ticks >= 0);
  c_module_level: cover property (@(posedge clk) ticks > 0);

  if (Checked) begin : gen_checked
    a_in_generate: assert property (@(posedge clk) gate)
      else $error("gate was low at a clock edge");
  end

  final begin
    if (ticks !== 4) $fatal(1, "ticks was %0d, expected 4", ticks);
    if (gate !== 1'b1) $fatal(1, "gate was %b, expected 1", gate);
    $display("All checks passed");
  end
endmodule
