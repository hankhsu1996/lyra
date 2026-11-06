// A clocking block samples each input at its clocking event as its input skew
// says (LRM 14.3, 14.4, 14.13): `#1step`, the default, takes the value the
// signal held at the end of the time step before the event; `#0` takes it in
// the Observed region of the event's own time step, after its nonblocking
// updates; a delay takes it as it stood at the end of the time step that long
// before the event, a skew written as a parameter being the instance's own. An
// inout reads as its input does. The samples are updated before the block's
// event is triggered, in Observed, so a process waiting on the block reads what
// was just sampled (LRM 14.10, 14.13), and an event control on an input waits
// for a change of its sampled value (LRM 14.15). A
// block a program declares samples in the same regions (LRM 14.13, 24.3.1).
program Watcher(input logic clk, input int d, output int now_seen,
                output int early_seen);
  timeunit 1ns;
  timeprecision 100ps;
  clocking pcb @(posedge clk);
    input #0 now = d;
    input #4 early = d;
  endclocking
  initial begin
    @(pcb);       // 10
    @(pcb);       // 30
    now_seen = pcb.now;
    early_seen = pcb.early;
    repeat (2) @(pcb);
  end
endprogram

// Samples `d` with a skew each instance sets.
module Sampler #(parameter int SK = 1) (input logic clk, input int d,
                                        output int seen);
  timeunit 1ns;
  timeprecision 100ps;
  clocking c @(posedge clk);
    input #SK x = d;
  endclocking
  initial begin
    @(c);         // 10
    @(c);         // 30
    seen = c.x;
  end
endmodule

// A skew is read in the time unit of the scope declaring the block, a fraction
// of it included (LRM 14.4, 3.14): at the rising edge at 5, a `#1.5` input
// reads the signal as it stood at 3.5.
module FractionalInputSkew(output logic [7:0] seen);
  timeunit 1ns;
  timeprecision 100ps;
  logic c = 0;
  always #5 c = ~c;
  logic [7:0] d = 0;
  clocking k @(posedge c);
    input #1.5 x = d;
  endclocking
  initial begin
    #3 d = 1;
    #0.7 d = 2;   // 3.7
  end
  initial begin
    @(k);         // 5
    seen = k.x;
  end
endmodule

module Top;
  timeunit 1ns;
  timeprecision 100ps;
  logic clk = 0;
  int d = 0;
  int q = 0;
  always #10 clk = ~clk;

  clocking cb @(posedge clk);
    input d;
    input #0 now = d;
    input #4 early = d;
    inout q;
  endclocking

  // The values each input holds after the block's events at 30 and at 50.
  int d_at_30 = -1, now_at_30 = -1, early_at_30 = -1, q_at_30 = -1;
  int d_at_50 = -1, now_at_50 = -1, early_at_50 = -1;
  time first_change = 0;
  // What the program's block sampled, through its ports.
  int program_now, program_early;
  // What two instances with different skews sampled at 30.
  int two_back, six_back;
  bit done = 0;

  Watcher watcher(clk, d, program_now, program_early);
  Sampler #(.SK(2)) sampler_2(clk, d, two_back);
  Sampler #(.SK(6)) sampler_6(clk, d, six_back);
  logic [7:0] fractional_seen;
  FractionalInputSkew fractional(fractional_seen);

  // An edge of an input's sampled value, which changes only at the block's
  // events (LRM 14.15).
  logic b = 0;
  time b_rose = 0;
  clocking cbb @(posedge clk);
    input b;
  endclocking
  initial #21 b = 1;      // sampled 1 at the event at 30
  initial begin
    @(posedge cbb.b);
    b_rose = $time;
  end

  // A signal changed in the time step of its clock's edge, before the edge:
  // `#1step` takes the value from before that time step.
  logic clk2 = 0;
  int d2 = 0;
  int d2_seen = -1;
  clocking cb2 @(posedge clk2);
    input d2;
  endclocking
  initial begin
    #15 d2 = 55;
    clk2 = 1;
  end
  initial begin
    @(cb2);
    d2_seen = cb2.d2;
  end

  initial begin
    #5 d = 1;
    q = 9;
    #21 d = 8;   // t = 26, the end of which a #4 input reads at 30
    #1 d = 2;    // t = 27
    #18 d = 4;   // t = 45
    #1 d = 6;    // t = 46, the end of which a #4 input reads at 50
    #2 d = 5;    // t = 48
  end

  // In the time step of the event at 30, before its Observed region.
  always @(posedge clk) if ($time == 30) d <= 3;

  initial begin
    @(cb);       // 10
    @(cb);       // 30
    d_at_30 = cb.d;
    now_at_30 = cb.now;
    early_at_30 = cb.early;
    q_at_30 = cb.q;
    @(cb);       // 50
    d_at_50 = cb.d;
    now_at_50 = cb.now;
    early_at_50 = cb.early;
    done = 1;
    $finish;
  end

  initial begin
    @(cb.d);
    first_change = $time;
  end

  final begin
    if (!done) $fatal(1, "the block's events were not all reached");
    if (d_at_30 !== 2) $fatal(1, "1step input at 30 was %0d, expected 2", d_at_30);
    if (now_at_30 !== 3) $fatal(1, "#0 input at 30 was %0d, expected 3", now_at_30);
    if (early_at_30 !== 8) $fatal(1, "#4 input at 30 was %0d, expected 8", early_at_30);
    if (q_at_30 !== 9) $fatal(1, "inout at 30 was %0d, expected 9", q_at_30);
    if (d_at_50 !== 5) $fatal(1, "1step input at 50 was %0d, expected 5", d_at_50);
    if (now_at_50 !== 5) $fatal(1, "#0 input at 50 was %0d, expected 5", now_at_50);
    if (early_at_50 !== 6) $fatal(1, "#4 input at 50 was %0d, expected 6", early_at_50);
    if (first_change !== 10) $fatal(1, "the sampled value first changed at %0t, expected 10", first_change);
    if (program_now !== 3) $fatal(1, "a program's #0 input at 30 was %0d, expected 3", program_now);
    if (program_early !== 8) $fatal(1, "a program's #4 input at 30 was %0d, expected 8", program_early);
    if (two_back !== 2) $fatal(1, "a #2 instance's input at 30 was %0d, expected 2", two_back);
    if (six_back !== 1) $fatal(1, "a #6 instance's input at 30 was %0d, expected 1", six_back);
    if (d2_seen !== 0) $fatal(1, "1step input at a same-step edge was %0d, expected 0", d2_seen);
    if (fractional_seen !== 8'h01) $fatal(1, "a #1.5 input read %h, expected 01", fractional_seen);
    if (b_rose !== 30) $fatal(1, "the sampled input rose at %0t, expected 30", b_rose);
    $display("All checks passed");
  end
endmodule
