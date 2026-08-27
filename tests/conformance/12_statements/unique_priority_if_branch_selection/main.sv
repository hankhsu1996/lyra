// The unique, unique0, and priority keywords written before an if add a
// violation check to an if-else-if chain and change nothing about which
// statement runs: whichever of them is written, the statement executed is the
// one belonging to the true condition appearing first, and no statement
// belonging to another true condition runs with it. When no condition is true
// and there is no else, no statement runs at all. A violation is detected
// while the statement executes but reported no earlier than the Observed
// region of that time step, so a chain that violates its check still hands
// control to the statement after it (LRM 12.4.2, 12.4.2.1).
module Top;
  int overlap_unique;
  int overlap_unique0;
  int overlap_priority;
  int single_true_unique;
  int single_true_unique0;
  int single_true_priority;
  int none_true_unique;
  int none_true_unique0;
  int none_true_priority;
  int reached_after_violation;

  initial begin
    int value;

    // Every condition holds, so a check on uniqueness is violated. The
    // statement that runs is still the first one and only the first one.
    value = 10;
    overlap_unique = 0;
    unique if (value > 0) overlap_unique = 1;
    else if (value > 5) overlap_unique = 2;
    else if (value > 9) overlap_unique = 3;

    overlap_unique0 = 0;
    unique0 if (value > 0) overlap_unique0 = 1;
    else if (value > 5) overlap_unique0 = 2;
    else if (value > 9) overlap_unique0 = 3;

    overlap_priority = 0;
    priority if (value > 0) overlap_priority = 1;
    else if (value > 5) overlap_priority = 2;
    else if (value > 9) overlap_priority = 3;

    // Exactly one condition holds, so nothing is violated and that one
    // condition's statement runs.
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

    // No condition holds and no else covers them, which unique and priority
    // check for and unique0 does not. Either way no statement runs.
    value = 99;
    none_true_unique = 7;
    unique if (value == 1) none_true_unique = 1;
    else if (value == 2) none_true_unique = 2;

    none_true_unique0 = 7;
    unique0 if (value == 1) none_true_unique0 = 1;
    else if (value == 2) none_true_unique0 = 2;

    none_true_priority = 7;
    priority if (value == 1) none_true_priority = 1;
    else if (value == 2) none_true_priority = 2;

    reached_after_violation = 5;
  end

  final begin
    if (overlap_unique !== 1)
      $fatal(1, "overlap_unique was %0d, expected 1", overlap_unique);
    if (overlap_unique0 !== 1)
      $fatal(1, "overlap_unique0 was %0d, expected 1", overlap_unique0);
    if (overlap_priority !== 1)
      $fatal(1, "overlap_priority was %0d, expected 1", overlap_priority);
    if (single_true_unique !== 2)
      $fatal(1, "single_true_unique was %0d, expected 2", single_true_unique);
    if (single_true_unique0 !== 2)
      $fatal(1, "single_true_unique0 was %0d, expected 2",
             single_true_unique0);
    if (single_true_priority !== 2)
      $fatal(1, "single_true_priority was %0d, expected 2",
             single_true_priority);
    if (none_true_unique !== 7)
      $fatal(1, "none_true_unique was %0d, expected 7", none_true_unique);
    if (none_true_unique0 !== 7)
      $fatal(1, "none_true_unique0 was %0d, expected 7", none_true_unique0);
    if (none_true_priority !== 7)
      $fatal(1, "none_true_priority was %0d, expected 7", none_true_priority);
    if (reached_after_violation !== 5)
      $fatal(1, "reached_after_violation was %0d, expected 5",
             reached_after_violation);
    $display("All checks passed");
  end
endmodule
