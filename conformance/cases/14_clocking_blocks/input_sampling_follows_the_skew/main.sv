// A clocking block samples each input at its clocking event as its input skew
// says (LRM 14.3, 14.4, 14.13): `#1step`, the default, takes the value the
// signal held at the end of the time step before the event; `#0` takes it in
// the Observed region of the event's own time step, after its nonblocking
// updates; a delay takes it as it stood at the end of the time step that long
// before the event, a skew written as a parameter being the instance's own. An
// inout reads as its input does, and a block a program declares samples in the
// same regions (LRM 14.13, 24.3.1). An event control on an input waits for a
// change of its sampled value (LRM 14.15).
//
// A sample other than a `#0` one changes before the block's event, which is
// triggered after it in Observed, whichever region the edge comes in -- a
// blocking write, a nonblocking one, a program's, a foreign callback's before
// the active events (LRM 14.10, 14.13). So a process waiting on the block reads
// what was just sampled, and a design process woken by such a sample changing,
// waiting on the block from there, is reached by the event of that same time
// step, however it reaches the block. A `#0` sample changes in Observed itself
// (LRM 14.4, 14.13), so a process its change wakes runs after that event and is
// reached by the next one.
program Watcher(input logic clk, input int d, output int now_seen,
                output int early_seen, output logic driven_clk);
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
  // A clock a program drives, which rises in the Reactive region.
  initial begin
    driven_clk = 0;
    forever #10 driven_clk = ~driven_clk;
  end
endprogram

// Samples `d` and `w` with a skew each instance sets.
module Sampler #(parameter int SK = 1) (input logic clk, input int d,
                                        input logic w, output int seen,
                                        output time woken);
  timeunit 1ns;
  timeprecision 100ps;
  clocking c @(posedge clk);
    input #SK x = d;
    input #SK y = w;
  endclocking
  initial begin
    @(c);         // 10
    @(c);         // 30
    seen = c.x;
  end
  initial begin
    @(posedge c.y);
    @(c);
    woken = $time;
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

// A block reached through a virtual interface from a class's task, as a
// verification component waits for a reset to be sampled and then for the
// next event.
interface SampledBus(input logic clk);
  timeunit 1ns;
  timeprecision 100ps;
  logic w = 0;
  clocking cb @(posedge clk);
    input w;
  endclocking
endinterface

class Monitor;
  virtual SampledBus vif;
  time woken = 0;

  function new(virtual SampledBus vif);
    this.vif = vif;
  endfunction

  task run();
    @(posedge vif.cb.w);
    @(vif.cb);
    woken = $time;
  endtask
endclass

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
  logic program_clk;
  // What three instances with different skews sampled at 30.
  int two_back, six_back, none_back;
  time woken_two_back, woken_six_back, woken_none_back;
  bit done = 0;

  // `w` rises at 12, so every skew below samples it as 1 at the event at 30.
  logic w = 0;
  initial #12 w = 1;

  Watcher watcher(clk, d, program_now, program_early, program_clk);
  Sampler #(.SK(2)) sampler_2(clk, d, w, two_back, woken_two_back);
  Sampler #(.SK(6)) sampler_6(clk, d, w, six_back, woken_six_back);
  Sampler #(.SK(0)) sampler_0(clk, d, w, none_back, woken_none_back);
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

  // A process woken by a sample of `w` changing, then waiting on the block:
  // the event of the same time step for every skew but `#0`.
  clocking cw @(posedge clk);
    input w;
    input #4 w_early = w;
    input #1.5 w_fraction = w;
    input #0 w_now = w;
  endclocking
  time woken_by_one_step = 0, woken_by_delay = 0, woken_by_fraction = 0;
  time woken_by_none = 0;
  initial begin
    @(posedge cw.w);
    @(cw);
    woken_by_one_step = $time;
  end
  initial begin
    @(posedge cw.w_early);
    @(cw);
    woken_by_delay = $time;
  end
  initial begin
    @(posedge cw.w_fraction);
    @(cw);
    woken_by_fraction = $time;
  end
  initial begin
    @(posedge cw.w_now);
    @(cw);
    woken_by_none = $time;
  end

  SampledBus bus(clk);
  Monitor monitor;
  initial #12 bus.w = 1;
  initial begin
    monitor = new(bus);
    monitor.run();
  end

  // Three more clocks, each rising at 10, 30 and 50, whose edge comes in
  // another region: a nonblocking write's, the program's, and a foreign time
  // callback's in Pre-Active (LRM 4.4.2, Table 4-1). Each block samples a
  // signal changed exactly its skew before the edge at 30, and one that
  // changed long before.
  logic nba_clk = 0;
  always #10 nba_clk <= ~nba_clk;
  logic callback_clk = 0;
  import "DPI-C" context function void drive_callback_clock();
  initial drive_callback_clock();

  int skewed = 0;
  initial #26 skewed = 7;

  clocking nba_cb @(posedge nba_clk);
    input #4 x = skewed;
    input w;
  endclocking
  clocking program_cb @(posedge program_clk);
    input #4 x = skewed;
    input w;
  endclocking
  clocking callback_cb @(posedge callback_clk);
    input #4 x = skewed;
    input w;
  endclocking

  int nba_seen = -1, program_seen = -1, callback_seen = -1;
  time woken_at_nba = 0, woken_at_program = 0, woken_at_callback = 0;
  initial begin
    @(nba_cb);
    @(nba_cb);
    nba_seen = nba_cb.x;
  end
  initial begin
    @(program_cb);
    @(program_cb);
    program_seen = program_cb.x;
  end
  initial begin
    @(callback_cb);
    @(callback_cb);
    callback_seen = callback_cb.x;
  end
  initial begin
    @(posedge nba_cb.w);
    @(nba_cb);
    woken_at_nba = $time;
  end
  initial begin
    @(posedge program_cb.w);
    @(program_cb);
    woken_at_program = $time;
  end
  initial begin
    @(posedge callback_cb.w);
    @(callback_cb);
    woken_at_callback = $time;
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
    // Past every process the events at 50 reach.
    #1 $finish;
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
    if (none_back !== 3) $fatal(1, "a #0 instance's input at 30 was %0d, expected 3", none_back);
    if (d2_seen !== 0) $fatal(1, "1step input at a same-step edge was %0d, expected 0", d2_seen);
    if (fractional_seen !== 8'h01) $fatal(1, "a #1.5 input read %h, expected 01", fractional_seen);
    if (b_rose !== 30) $fatal(1, "the sampled input rose at %0t, expected 30", b_rose);
    if (woken_by_one_step !== 30) $fatal(1, "woken by a 1step sample, the block's event came at %0t, expected 30", woken_by_one_step);
    if (woken_by_delay !== 30) $fatal(1, "woken by a #4 sample, the block's event came at %0t, expected 30", woken_by_delay);
    if (woken_by_fraction !== 30) $fatal(1, "woken by a #1.5 sample, the block's event came at %0t, expected 30", woken_by_fraction);
    if (woken_by_none !== 50) $fatal(1, "woken by a #0 sample, the block's event came at %0t, expected 50", woken_by_none);
    if (woken_two_back !== 30) $fatal(1, "woken by a #2 instance's sample, the block's event came at %0t, expected 30", woken_two_back);
    if (woken_six_back !== 30) $fatal(1, "woken by a #6 instance's sample, the block's event came at %0t, expected 30", woken_six_back);
    if (woken_none_back !== 50) $fatal(1, "woken by a #0 instance's sample, the block's event came at %0t, expected 50", woken_none_back);
    if (monitor.woken !== 30) $fatal(1, "woken through a virtual interface, the block's event came at %0t, expected 30", monitor.woken);
    if (nba_seen !== 7) $fatal(1, "a #4 input of a nonblocking clock at 30 was %0d, expected 7", nba_seen);
    if (program_seen !== 7) $fatal(1, "a #4 input of a program's clock at 30 was %0d, expected 7", program_seen);
    if (callback_seen !== 7) $fatal(1, "a #4 input of a callback's clock at 30 was %0d, expected 7", callback_seen);
    if (woken_at_nba !== 30) $fatal(1, "woken at a nonblocking clock, the block's event came at %0t, expected 30", woken_at_nba);
    if (woken_at_program !== 30) $fatal(1, "woken at a program's clock, the block's event came at %0t, expected 30", woken_at_program);
    if (woken_at_callback !== 30) $fatal(1, "woken at a callback's clock, the block's event came at %0t, expected 30", woken_at_callback);
    $display("All checks passed");
  end
endmodule
