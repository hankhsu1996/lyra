// Each continuous assignment is a process of its own (LRM 4.9.1, Annex P), so
// a function its expression calls runs in that process: process::self() there
// names it, the same process every time the assignment is evaluated, one that
// is RUNNING while it evaluates (LRM 9.7) and not the process that changed
// what it reads.
module Top;
  logic [3:0] a;
  wire [3:0] y;
  process assignment, writer;
  int evaluations;
  int named_itself, running_inside, same_process, not_the_writer;

  function automatic logic [3:0] follow(logic [3:0] v);
    process p = process::self();
    evaluations++;
    if (assignment == null) begin
      assignment = p;
    end else begin
      same_process = (p == assignment);
    end
    named_itself = (p != null);
    running_inside = (p.status() == process::RUNNING);
    return v;
  endfunction

  assign y = follow(a);

  initial begin
    writer = process::self();
    a = 4'd3;
    #1;
    not_the_writer = (assignment != writer);
    a = 4'd5;
    #1;
  end

  final begin
    if (evaluations < 2)
      $fatal(1, "evaluations was %0d, expected at least 2", evaluations);
    if (named_itself !== 1)
      $fatal(1, "named_itself was %0d, expected 1", named_itself);
    if (running_inside !== 1)
      $fatal(1, "running_inside was %0d, expected 1", running_inside);
    if (same_process !== 1)
      $fatal(1, "same_process was %0d, expected 1", same_process);
    if (not_the_writer !== 1)
      $fatal(1, "not_the_writer was %0d, expected 1", not_the_writer);
    if (y !== 4'd5) $fatal(1, "y was %0d, expected 5", y);
    $display("All checks passed");
  end
endmodule
