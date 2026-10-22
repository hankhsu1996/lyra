// A final procedure occurs at the end of simulation time (LRM 9.2.3) and
// $finish reaches that end (LRM 20.2), whatever the live processes were doing.
// One of them here is suspended part way through a foreign call that consumes
// simulation time (LRM 35.5.1.1), so its continuation is buried under a native
// call stack; the run still ends, the final procedure still runs, and what the
// simulation computed before the end stands.
//
// The stack is released where it stands, and every frame of the design's on it
// ends what it holds on the way: the string the import call made for its
// argument, and the string the exported task was handed. Each is longer than a
// string keeps in place, so one not ended is storage never given back, which a
// run checking its memory reports.
module Top;
  import "DPI-C" context task advance(input string tag, input int rounds);

  export "DPI-C" task step;

  int count;
  int ended_at;
  int label_length;

  task step(input string label, input int amount);
    label_length = label.len();
    #amount;
    count = (count * 10) + amount;
  endtask

  initial begin
    string tag;
    count = 0;
    tag = "a tag longer than a string keeps in place";
    advance({tag, ", joined to another"}, 3);
    count = 9999;
  end

  initial begin
    ended_at = -1;
    // Lands while the second exported task is suspended on its delay.
    #4;
    ended_at = $time;
    $finish;
  end

  final begin
    if (count !== 2) $fatal(1, "count was %0d, expected 2", count);
    if (ended_at !== 4)
      $fatal(1, "the run ended at %0d, expected 4", ended_at);
    if (label_length !== 51)
      $fatal(1, "the label was %0d long, expected 51", label_length);
    $display("All checks passed");
  end
endmodule
