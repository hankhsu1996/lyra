// @reports-none: violation
//
// The unique, unique0, and priority keywords written before an if add a
// violation check to an if-else-if chain and change nothing about which
// statement runs. Where exactly one condition is true, that condition's
// statement runs whichever keyword is written. A priority-if asserts only that
// some condition is true, so several true ones break nothing and the first of
// them acts. A unique0-if does not assert that any condition is true, so where
// none is and there is no else, no statement runs and nothing is violated
// (LRM 12.4.2). Each execution here satisfies its check, so no violation
// report is issued; the executions that violate one are cases of their own.
module Top;
  int single_true_unique;
  int single_true_unique0;
  int single_true_priority;
  int overlap_priority;
  int none_true_unique0;

  initial begin
    int value;

    value = 2;
    single_true_unique = 0;
    unique if (value == 1) single_true_unique = 1;
    else if (value == 2) single_true_unique = 2;
    else if (value == 3) single_true_unique = 3;

    single_true_unique0 = 0;
    unique0 if (value == 1) single_true_unique0 = 1;
    else if (value == 2) single_true_unique0 = 2;
    else if (value == 3) single_true_unique0 = 3;

    single_true_priority = 0;
    priority if (value == 1) single_true_priority = 1;
    else if (value == 2) single_true_priority = 2;
    else if (value == 3) single_true_priority = 3;

    // Every condition holds.
    value = 10;
    overlap_priority = 0;
    priority if (value > 0) overlap_priority = 1;
    else if (value > 5) overlap_priority = 2;
    else if (value > 9) overlap_priority = 3;

    value = 99;
    none_true_unique0 = 7;
    unique0 if (value == 1) none_true_unique0 = 1;
    else if (value == 2) none_true_unique0 = 2;

    if (single_true_unique !== 2)
      $fatal(1, "single_true_unique was %0d, expected 2", single_true_unique);
    if (single_true_unique0 !== 2)
      $fatal(1, "single_true_unique0 was %0d, expected 2",
             single_true_unique0);
    if (single_true_priority !== 2)
      $fatal(1, "single_true_priority was %0d, expected 2",
             single_true_priority);
    if (overlap_priority !== 1)
      $fatal(1, "overlap_priority was %0d, expected 1", overlap_priority);
    if (none_true_unique0 !== 7)
      $fatal(1, "none_true_unique0 was %0d, expected 7", none_true_unique0);
    #1 $display("All checks passed");
  end
endmodule
