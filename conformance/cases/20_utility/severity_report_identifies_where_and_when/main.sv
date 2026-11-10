// @reports: info line=40 time=37 scope=Top.probe message="a probe report"
// @reports: info line=46 time=40 scope=Top.named message="in a named block"
// @reports: info line=48 scope=Top.named.inner message="in a nested block"
// @reports: info line=50 scope=Top.named.labeled message="in a labeled one"
// @reports: warning line=22 scope=Top.other.warn message="in another's task"
// @reports: info line=34 scope=Top.note message="in a function"
// @reports: error line=26 scope=Top.other.check
//
// Every severity system task prints a tool-specific message, and that message
// shall include the file name and line number of the call, the hierarchical
// name of the scope the call is made in, and the simulation run time at which
// it is made (LRM 20.10). The scope is where the call is written, so it is the
// one `%m` names there (LRM 21.2.1.5): the innermost named block or labeled
// statement around it (LRM 9.3.5), the task or function it sits in, and the
// instance holding that, whichever procedure made the call. A task of another
// instance called by its hierarchical name is that instance's (LRM 23.6), and
// an assertion with no fail action calls `$error` where it is written (LRM
// 20.10). A program cannot read a message about itself, so the claims above
// are that each report carries them.
module Other;
  task warn();
    $warning("in another's task");
  endtask

  task check(bit holds);
    assert (holds);
  endtask
endmodule

module Top;
  Other other ();

  function automatic void note();
    $info("in a function");
  endfunction

  if (1) begin : probe
    initial begin
      #37;
      $info("a probe report");
    end
  end

  initial begin : named
    #40;
    $info("in a named block");
    begin : inner
      $info("in a nested block");
    end
    labeled : $info("in a labeled one");
    other.warn();
    note();
    $display("Ready for the report");
    other.check(0);
    $display("All checks passed");
  end
endmodule
