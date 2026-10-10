// Two procedures under @* that each run a for loop over one variable declared
// outside both are ordinary in RTL written before a loop could declare its own,
// and such a design is expected to settle. Read to the letter, LRM 9.4.2.2
// lists every identifier the statement reads, the loop's condition reads its
// control variable, and so each procedure resumes on the other's writes of it
// and neither ever stops. This compiler leaves out of an @* list a variable the
// statement uses only to control its for loops: the loop assigns it before any
// read, so what it held before reaches nothing the statement computes. Nothing
// else is left out. A variable the statement writes and also reads is in the
// list as the clause has it, and so is a control variable the statement names
// outside its loop or reads in the loop's own initialization.
module Top;
  logic [3:0] in;
  logic [3:0] any_set, all_set;
  integer j;

  always @(*) begin : first
    any_set = 4'b0;
    for (j = 0; j < 4; j = j + 1) any_set = any_set | {4{in[j]}};
  end

  always @(*) begin : second
    all_set = 4'hF;
    for (j = 0; j < 4; j = j + 1) all_set = all_set & {4{in[j]}};
  end

  logic [3:0] counted_in, counted_acc;
  integer k;
  int control_runs;
  always @* begin
    control_runs = control_runs + 1;
    counted_acc = 4'b0;
    for (k = 0; k < 4; k = k + 1) counted_acc = counted_acc | {4{counted_in[k]}};
  end

  logic sel, a, y, z;
  int written_runs;
  always @* begin
    written_runs = written_runs + 1;
    if (sel) y = a;
    z = y;
  end

  logic [3:0] after_in;
  integer m;
  int after_runs, last;
  always @* begin
    after_runs = after_runs + 1;
    for (m = 0; m < 4; m = m + 1) last = after_in[m];
    last = m;
  end

  integer p;
  int init_runs, steps;
  always @* begin
    init_runs = init_runs + 1;
    steps = 0;
    for (p = p & 1; p < 4; p = p + 1) steps = steps + 1;
  end

  logic [3:0] seen_any, seen_all;
  int control_runs_before, written_runs_before, after_runs_before;
  int init_runs_before;

  initial begin
    control_runs = 0;
    written_runs = 0;
    after_runs = 0;
    init_runs = 0;
    p = 0;
    sel = 0;
    a = 0;
    y = 0;
    counted_in = 4'b0001;
    after_in = 4'b0001;
    in = 4'b0100;
    #1;
    seen_any = any_set;
    seen_all = all_set;
    control_runs_before = control_runs;
    written_runs_before = written_runs;
    after_runs_before = after_runs;
    init_runs_before = init_runs;
    in = 4'b1111;
    k = 9;
    y = 1;
    m = 9;
    p = 9;
    #1;
  end

  final begin
    if (seen_any !== 4'hF) $fatal(1, "any_set first was %h, expected f", seen_any);
    if (seen_all !== 4'h0) $fatal(1, "all_set first was %h, expected 0", seen_all);
    if (any_set !== 4'hF) $fatal(1, "any_set was %h, expected f", any_set);
    if (all_set !== 4'hF) $fatal(1, "all_set was %h, expected f", all_set);
    if (control_runs !== control_runs_before)
      $fatal(1, "a write of the loop control variable ran its statement again");
    if (written_runs !== written_runs_before + 1)
      $fatal(1, "a write of a variable the statement also writes ran it %0d more times, expected 1",
             written_runs - written_runs_before);
    if (z !== 1'b1) $fatal(1, "z was %b, expected 1", z);
    if (after_runs !== after_runs_before + 1)
      $fatal(1, "a write of a control variable named outside its loop ran the statement %0d more times, expected 1",
             after_runs - after_runs_before);
    if (init_runs !== init_runs_before + 1)
      $fatal(1, "a write of a control variable its loop's initialization reads ran the statement %0d more times, expected 1",
             init_runs - init_runs_before);
    $display("All checks passed");
  end
endmodule
