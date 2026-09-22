// $cast assigns a source expression to a destination variable whose declared
// type would not ordinarily allow it, and decides while the program runs
// whether that particular assignment is valid. An integral value becomes an
// enumeration only where it is one of the declared members, so validity depends
// on the value rather than on the two types; where the types alone settle it
// the answer is the same every time. Called as a function it answers 1 or 0 and
// issues no error, and an invalid assignment leaves the destination unchanged;
// called as a task it performs the same assignment. The positions that discard
// the answer are a statement and a for-loop step, and a void cast of the
// function call discards it without asking for an error
// (LRM 6.24.2, 6.24.1, 6.22.3, 6.22.4, 6.19, A.6.8).
module Top;
  typedef enum int {RED = 1, GREEN = 2, BLUE = 4} Colour;

  int answer_for_member;
  int answer_for_non_member;
  int colour_after_member;
  int colour_after_non_member;
  int colour_from_task;
  int colour_from_void_cast;
  int answer_for_always_admissible;
  int int_from_real;
  int answer_for_never_admissible;
  int int_after_never_admissible;
  int reached_the_negated_branch;
  int colour_after_negated_branch;
  int colour_from_valid_loop_step;
  int colour_after_invalid_loop_step;
  int steps_taken;

  initial begin
    Colour c;
    int raw;
    real r;
    chandle opaque;
    int i;

    // A value the enumeration declares.
    c = RED;
    colour_after_member = 0;
    raw = 4;
    answer_for_member = $cast(c, raw);
    colour_after_member = int'(c);

    // A value it does not declare: the destination keeps what it held.
    c = GREEN;
    colour_after_non_member = 0;
    raw = 3;
    answer_for_non_member = $cast(c, raw);
    colour_after_non_member = int'(c);

    // The task spelling performs the same assignment.
    c = RED;
    colour_from_task = 0;
    raw = 2;
    $cast(c, raw);
    colour_from_task = int'(c);

    // The function spelling with its answer discarded.
    c = RED;
    colour_from_void_cast = 0;
    raw = 4;
    void'($cast(c, raw));
    colour_from_void_cast = int'(c);

    // Two types one of which is assignment compatible with the other: the
    // answer does not depend on the value.
    r = 6.25;
    int_from_real = 0;
    answer_for_always_admissible = $cast(raw, r);
    int_from_real = raw;

    // Types between which the standard defines no conversion at all: a chandle
    // is type incompatible with every other type, so no value is admissible.
    opaque = null;
    raw = 11;
    answer_for_never_admissible = $cast(raw, opaque);
    int_after_never_admissible = raw;

    // The answer read where the condition of a statement is, which is the
    // spelling the standard's own example uses.
    c = BLUE;
    reached_the_negated_branch = 0;
    raw = 9;
    if (!$cast(c, raw)) reached_the_negated_branch = 1;
    colour_after_negated_branch = int'(c);

    // A loop step discards the answer the way a statement does, and the loop
    // runs its steps once per iteration.
    c = RED;
    raw = 2;
    steps_taken = 0;
    colour_from_valid_loop_step = 0;
    for (i = 0; i < 3; i = i + 1, $cast(c, raw)) steps_taken = steps_taken + 1;
    colour_from_valid_loop_step = int'(c);

    c = GREEN;
    raw = 9;
    for (i = 0; i < 1; i = i + 1, $cast(c, raw)) begin
    end
    colour_after_invalid_loop_step = int'(c);
  end

  final begin
    if (answer_for_member !== 1)
      $fatal(1, "answer_for_member was %0d, expected 1", answer_for_member);
    if (colour_after_member !== 4)
      $fatal(1, "colour_after_member was %0d, expected 4", colour_after_member);
    if (answer_for_non_member !== 0)
      $fatal(1, "answer_for_non_member was %0d, expected 0",
             answer_for_non_member);
    if (colour_after_non_member !== 2)
      $fatal(1, "colour_after_non_member was %0d, expected 2",
             colour_after_non_member);
    if (colour_from_task !== 2)
      $fatal(1, "colour_from_task was %0d, expected 2", colour_from_task);
    if (colour_from_void_cast !== 4)
      $fatal(1, "colour_from_void_cast was %0d, expected 4",
             colour_from_void_cast);
    if (answer_for_always_admissible !== 1)
      $fatal(1, "answer_for_always_admissible was %0d, expected 1",
             answer_for_always_admissible);
    if (int_from_real !== 6)
      $fatal(1, "int_from_real was %0d, expected 6", int_from_real);
    if (answer_for_never_admissible !== 0)
      $fatal(1, "answer_for_never_admissible was %0d, expected 0",
             answer_for_never_admissible);
    if (int_after_never_admissible !== 11)
      $fatal(1, "int_after_never_admissible was %0d, expected 11",
             int_after_never_admissible);
    if (reached_the_negated_branch !== 1)
      $fatal(1, "reached_the_negated_branch was %0d, expected 1",
             reached_the_negated_branch);
    if (colour_after_negated_branch !== 4)
      $fatal(1, "colour_after_negated_branch was %0d, expected 4",
             colour_after_negated_branch);
    if (steps_taken !== 3)
      $fatal(1, "steps_taken was %0d, expected 3", steps_taken);
    if (colour_from_valid_loop_step !== 2)
      $fatal(1, "colour_from_valid_loop_step was %0d, expected 2",
             colour_from_valid_loop_step);
    if (colour_after_invalid_loop_step !== 2)
      $fatal(1, "colour_after_invalid_loop_step was %0d, expected 2",
             colour_after_invalid_loop_step);
    $display("All checks passed");
  end
endmodule
