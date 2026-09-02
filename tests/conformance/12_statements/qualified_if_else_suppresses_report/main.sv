// @reports-nothing:
//
// A violation report is issued when no condition of a unique-if or priority-if
// matches, unless there is an explicit else. An else covers every value the
// conditions left, so a chain carrying one asserts nothing about whether some
// condition holds and no report is issued however the conditions come out. A
// unique0-if never reports a no-match either way, so all three forms are
// silent here (LRM 12.4.2).
module Top;
  int unique_taken;
  int unique0_taken;
  int priority_taken;

  initial begin
    int value;
    value = 99;

    unique_taken = 0;
    unique if (value == 1) unique_taken = 1;
    else if (value == 2) unique_taken = 2;
    else unique_taken = 7;

    unique0_taken = 0;
    unique0 if (value == 1) unique0_taken = 1;
    else if (value == 2) unique0_taken = 2;
    else unique0_taken = 7;

    priority_taken = 0;
    priority if (value == 1) priority_taken = 1;
    else if (value == 2) priority_taken = 2;
    else priority_taken = 7;
  end

  final begin
    if (unique_taken !== 7)
      $fatal(1, "unique_taken was %0d, expected 7", unique_taken);
    if (unique0_taken !== 7)
      $fatal(1, "unique0_taken was %0d, expected 7", unique0_taken);
    if (priority_taken !== 7)
      $fatal(1, "priority_taken was %0d, expected 7", priority_taken);
    $display("All checks passed");
  end
endmodule
