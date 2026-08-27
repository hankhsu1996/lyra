// The name() method returns, as a string, the name standing for the value the
// variable holds. A value that is not one of the enumeration's names has no
// name, so name() returns the empty string for it (LRM 6.19.5.6).
module Top;
  typedef enum {IDLE = 2, RUN = 4, STOP = 8} state_t;

  state_t s;
  string run_name;
  string stop_name;
  string undeclared_name = "unset";

  initial begin
    s = RUN;
    run_name = s.name();
    s = STOP;
    stop_name = s.name();
    s = state_t'(5);
    undeclared_name = s.name();
  end

  final begin
    if (run_name !== "RUN")
      $fatal(1, "run_name was '%s', expected RUN", run_name);
    if (stop_name !== "STOP")
      $fatal(1, "stop_name was '%s', expected STOP", stop_name);
    if (undeclared_name !== "")
      $fatal(1, "undeclared_name was '%s', expected the empty string",
             undeclared_name);
    $display("All checks passed");
  end
endmodule
