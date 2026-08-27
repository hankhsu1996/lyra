// A zero delay does not advance simulation time; it suspends the procedure and
// schedules its resumption in the Inactive region of the same time slot, which
// is reached only once every event in the Active region has been processed
// (LRM 9.4.1, 4.4.2.3). A procedure that has passed one zero delay therefore
// observes everything the procedures still runnable in that region did, and a
// second zero delay puts it behind everything one zero delay let through.
module Top;
  int written_without_delay;
  int written_after_one_zero;

  int seen_after_one_zero;
  int seen_after_two_zeros;
  time time_after_two_zeros;

  initial begin
    #0;
    seen_after_one_zero = written_without_delay;
    #0;
    seen_after_two_zeros = written_after_one_zero;
    time_after_two_zeros = $time;
  end

  initial written_without_delay = 1;

  initial begin
    #0;
    written_after_one_zero = 1;
  end

  final begin
    if (seen_after_one_zero !== 1)
      $fatal(1, "seen_after_one_zero was %0d, expected 1",
             seen_after_one_zero);
    if (seen_after_two_zeros !== 1)
      $fatal(1, "seen_after_two_zeros was %0d, expected 1",
             seen_after_two_zeros);
    if (time_after_two_zeros !== 0)
      $fatal(1, "time_after_two_zeros was %0d, expected 0",
             time_after_two_zeros);
    $display("All checks passed");
  end
endmodule
