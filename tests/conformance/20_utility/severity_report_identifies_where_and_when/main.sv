// @reports: Top.probe at time 37
//
// Every severity system task prints a tool-specific message, and that message
// shall include the file name and line number of the call, the hierarchical
// name of the scope the call is made in, and the simulation run time at which
// it is made (LRM 20.10). A program cannot read a message about itself, so the
// two the program can name for itself are checked here -- the scope `%m`
// resolves to (LRM 21.2.1.5) and the time `$time` returns -- and the claim
// above is that the report carries them.
module Top;
  string scope_name;
  int reported_at;

  if (1) begin : probe
    initial begin
      scope_name = "not yet reported";
      reported_at = 9;
      #37;
      scope_name = $sformatf("%m");
      reported_at = $time;
      $info("a probe report");
    end
  end

  final begin
    if (scope_name != "Top.probe")
      $fatal(1, "the reporting scope was '%s', expected Top.probe",
             scope_name);
    if (reported_at !== 37)
      $fatal(1, "the report was made at %0d, expected 37", reported_at);
    $display("All checks passed");
  end
endmodule
