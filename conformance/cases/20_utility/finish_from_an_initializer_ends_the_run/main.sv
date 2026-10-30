// A variable declaration assignment is evaluated at time zero, before any
// procedure starts (LRM 10.5), and it may call a function that calls $finish
// (LRM 13.4), which ends the simulation where it is reached (LRM 20.2). So the
// function does not return, the variable keeps its default, no initial
// procedure runs at all, and a final procedure still runs because simulation
// ended by an explicit $finish (LRM 9.2.3).
module Top;
  int after_in_function;
  int started;

  function automatic int ends(int d);
    if (d == 0) $finish(0);
    after_in_function = 1;
    return d + 5;
  endfunction

  int initialized = ends(0);

  initial started = 1;

  final begin
    if (after_in_function !== 0)
      $fatal(1, "the function that called $finish carried on");
    if (initialized !== 0)
      $fatal(1, "the declaration assignment completed with %0d", initialized);
    if (started !== 0) $fatal(1, "an initial procedure ran after $finish");
    $display("All checks passed");
  end
endmodule
