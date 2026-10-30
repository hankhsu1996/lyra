// A blocking procedural assignment is executed before the statements that
// follow it in a sequential block, so every later statement in the block reads
// the value it has just placed in the variable and never the previous one
// (LRM 10.4.1). The right-hand side is any expression that evaluates to a
// value, including one that reads a variable an earlier statement in the same
// block assigned (LRM 10.2).
module Top;
  int direct;
  int computed;
  int chained;
  int negative;
  int from_negative;
  int reassigned;

  initial begin
    direct = 1;
    computed = 1 + 2;
    chained = direct + computed;
    negative = -3;
    from_negative = negative - 4;
    reassigned = 5;
    reassigned = reassigned * 2;
  end

  final begin
    if (direct !== 1) $fatal(1, "direct was %0d, expected 1", direct);
    if (computed !== 3) $fatal(1, "computed was %0d, expected 3", computed);
    if (chained !== 4) $fatal(1, "chained was %0d, expected 4", chained);
    if (negative !== -3)
      $fatal(1, "negative was %0d, expected -3", negative);
    if (from_negative !== -7)
      $fatal(1, "from_negative was %0d, expected -7", from_negative);
    if (reassigned !== 10)
      $fatal(1, "reassigned was %0d, expected 10", reassigned);
    $display("All checks passed");
  end
endmodule
