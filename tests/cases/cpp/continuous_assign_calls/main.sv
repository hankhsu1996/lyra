module Top;
  function automatic int dbl(int x);
    return x * 2;
  endfunction

  logic [3:0] a;
  int fn_result;
  logic [11:0] rep;
  bit in_set;
  int trig;
  int stamped;

  // Simulation-time value expressions in continuous-assignment position: a user
  // function call (LRM 13.4), a replication (LRM 11.4.12.1), and the inside
  // operator (LRM 11.4.13). All lower through the same context-free handlers as
  // their procedural counterparts.
  assign fn_result = dbl(a);
  assign rep = {3{a}};
  assign in_set = (a inside {4'd1, 4'd5, 4'd9});

  // A time read (LRM 20.3) is a pure value query -- it sequences nothing -- so
  // it is legal here, unlike the system subroutines that are effects. Driving
  // `trig` at time 7 re-evaluates the assignment, so the time it reads is the
  // current one (scaled to this scope's time unit, LRM 3.14.2) rather than the
  // time-zero evaluation.
  assign stamped = trig + $stime;

  // The other pure value queries legal here: a plusargs test (LRM 21.6) and
  // `$sformatf` (LRM 21.3.3). Their output-bearing siblings ($value$plusargs,
  // $sformat) carry an output argument, which the frontend keeps out of a
  // structural context entirely.
  bit has_arg;
  string label;
  assign has_arg = $test$plusargs("no_such_switch");
  assign label = $sformatf("a=%0d", a);

  initial begin
    a = 4'd5;
    #7;
    trig = 1;
  end
endmodule
