// @reports: violation
//
// With no else to cover the remaining values, a unique-if and a priority-if
// each assert that some condition holds, so an execution where none does is a
// violation and is reported. No statement runs either way, which is what
// separates the report from the selection (LRM 12.4.2).
module Top;
  int unique_taken;
  int priority_taken;

  initial begin
    int value;
    value = 99;

    unique_taken = 7;
    unique if (value == 1) unique_taken = 1;
    else if (value == 2) unique_taken = 2;

    priority_taken = 7;
    priority if (value == 1) priority_taken = 1;
    else if (value == 2) priority_taken = 2;
  end

  final begin
    if (unique_taken !== 7)
      $fatal(1, "unique_taken was %0d, expected 7", unique_taken);
    if (priority_taken !== 7)
      $fatal(1, "priority_taken was %0d, expected 7", priority_taken);
    $display("All checks passed");
  end
endmodule
