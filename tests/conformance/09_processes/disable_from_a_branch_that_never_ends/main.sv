// Disabling a named block terminates every activity enabled within it and
// execution resumes after the block (LRM 9.6.2), and the disable may come from
// inside one of those activities. Here it comes from a fork branch that loops
// forever, has waited, and holds automatic variables of its own: the branch
// ends at the disable, nothing after the fork inside the disabled block runs,
// and the rest of the run goes on as though nothing had been cut short.
module Top;
  int rounds;
  int resumed_at;
  int later;
  string last;

  initial begin : outer
    fork
      forever begin
        automatic string note = $sformatf("round %0d", rounds);
        #1;
        rounds = rounds + 1;
        last = note;
        if (rounds == 2) disable outer;
      end
    join
    resumed_at = -1;
  end

  initial begin
    #5;
    later = rounds;
  end

  final begin
    if (rounds !== 2) $fatal(1, "rounds was %0d, expected 2", rounds);
    if (resumed_at !== 0)
      $fatal(1, "resumed_at was %0d, expected 0: the disabled block went on",
             resumed_at);
    if (last != "round 1")
      $fatal(1, "last was \"%s\", expected \"round 1\"", last);
    if (later !== 2) $fatal(1, "later was %0d, expected 2", later);
    $display("All checks passed");
  end
endmodule
