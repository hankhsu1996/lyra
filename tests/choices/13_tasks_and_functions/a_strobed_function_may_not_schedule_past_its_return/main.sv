// @reports: error line=14
//
// A function may schedule what takes effect after it returns only on a thread
// an initial or always procedure created, or a fork of one, and it is an error
// otherwise (LRM 13.4.4). The standard names no thread that evaluates a
// strobed write's arguments (LRM 21.2.2); Lyra evaluates them on no thread, so
// a nonblocking assignment in a function one of them calls is that error, at
// the assignment. It could not take effect anyway: nothing may be scheduled
// into an earlier region once the Postponed region is reached (LRM 4.4.2.9).
module Top;
  int later;

  function automatic int schedules();
    later <= 1;
    return 3;
  endfunction

  initial begin
    $display("Ready for the report");
    $strobe("%0d", schedules());
    #1 $display("All checks passed");
  end
endmodule
