// A DPI call chain re-enters the foreign side from inside an exported task: the
// exported task an imported task called may itself call an imported task, so
// the chain is foreign, SystemVerilog, foreign and SystemVerilog again while
// the exported task between them suspends (LRM 35.5.1.5, 35.7, 35.8). Each
// call returns to the frame that made it, at any depth: an inner call that
// consumes no simulation time leaves the outer one exactly where it was, still
// able to suspend afterwards, and one that suspends, directly or in a task the
// exported one called, resumes the outer call where it was when it returns.
// Such a chain disabled or killed while it waits ends there (LRM 35.9, 9.6),
// and the run still ends at $finish (LRM 20.2) while one is suspended.
module Top;
  import "DPI-C" context task outer_call(input int amount);
  import "DPI-C" context task inner_call(input int value);
  import "DPI-C" context task outer_c(input int which);
  import "DPI-C" context task inner_c(input int which);
  import "DPI-C" context task deepest_c();

  export "DPI-C" task middle;
  export "DPI-C" function note;
  export "DPI-C" task relay;
  export "DPI-C" task leaf;
  export "DPI-C" task bottom;

  int trace;
  int noted;
  int finished_at;
  int trail[$];

  // Folding rather than adding makes the order of the two halves part of the
  // total, so a chain that came back to the wrong frame moves it.
  task middle(input int amount);
    #amount;
    trace = (trace * 10) + 1;
    inner_call(8);
    #amount;
    trace = (trace * 10) + 2;
  endtask

  function void note(input int value);
    noted = value;
  endfunction

  // The chains below run at once, so their tasks are automatic: a static
  // task's arguments would be one variable every chain shares (LRM 13.3.1).
  task automatic through(input int which);
    #1;
    inner_c(which);
    trail.push_back(which * 10 + 2);
  endtask

  task automatic relay(input int which);
    #1;
    through(which);
    trail.push_back(which * 10 + 3);
  endtask

  task automatic leaf(input int which);
    #5;
    if (which == 3) deepest_c();
    // Still waiting when the run ends, two foreign calls deep.
    if (which == 4) #100;
    trail.push_back(which * 10 + 1);
  endtask

  task automatic bottom();
    #2;
    trail.push_back(30);
  endtask

  initial begin
    trace = 0;
    noted = -1;
    outer_call(3);
    finished_at = $time;
  end

  initial begin
    outer_c(2);
    trail.push_back(24);
  end

  initial begin
    outer_c(3);
    trail.push_back(34);
  end

  initial begin
    outer_c(4);
    trail.push_back(44);
  end

  // Disabled while waiting two foreign calls deep: each foreign frame returns
  // the disable on (LRM 35.9), and the block ends where it was disabled.
  initial begin
    fork
      begin : disabled
        outer_c(5);
        trail.push_back(54);
      end
      begin
        #3;
        disable disabled;
      end
    join
    trail.push_back(59);
  end

  // Killed while waiting there; the process that killed it goes on.
  initial begin
    fork
      begin
        outer_c(6);
        trail.push_back(64);
      end
    join_none
    #4;
    disable fork;
    trail.push_back(69);
  end

  initial begin
    #20;
    $finish;
  end

  final begin
    automatic int expected[$] = '{59, 69, 21, 22, 23, 24, 30, 31, 32, 33, 34};
    if (trace !== 12) $fatal(1, "trace was %0d, expected 12", trace);
    if (noted !== 9) $fatal(1, "the inner call noted %0d, expected 9", noted);
    if (finished_at !== 6)
      $fatal(1, "the chain returned at time %0d, expected 6", finished_at);
    if (trail !== expected)
      $fatal(1, "the chains that suspend went on as %p", trail);
    $display("All checks passed");
  end
endmodule
