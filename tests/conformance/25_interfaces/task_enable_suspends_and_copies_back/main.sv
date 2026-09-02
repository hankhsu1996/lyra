// A task an interface declares is enabled through a port like any of its
// subroutines (LRM 25.7), and being a task it may consume time: the enable
// suspends the enabling process until the task completes (LRM 13.3), and an
// `output` formal is copied to the caller's actual at that completion rather
// than when the enable is written (LRM 13.5.2). So a value the task reads after
// its own delay is what the caller receives, and the statement after the enable
// runs at the time the task finished.
interface Bus;
  logic [7:0] data;
  int         completions;

  task automatic Fetch(output logic [7:0] value);
    #2;
    value = data;
    completions = completions + 1;
  endtask

  modport reader(import Fetch);
endinterface

module Fetcher (
    Bus.reader r
);
  logic [7:0] fetched = 8'h00;
  int         resumed_at = 0;

  initial begin
    #1 r.Fetch(fetched);
    resumed_at = $time;
  end
endmodule

module Top;
  Bus bus ();

  Fetcher fetcher (bus.reader);

  // Written after the enable and before the task completes, so a copy taken at
  // either moment other than completion yields something the checks reject.
  initial #2 bus.data = 8'ha5;

  final begin
    if (fetcher.fetched !== 8'ha5)
      $fatal(1, "fetched was %h, expected a5", fetcher.fetched);
    if (fetcher.resumed_at !== 3)
      $fatal(1, "the caller resumed at %0d, expected 3", fetcher.resumed_at);
    if (bus.completions !== 1)
      $fatal(1, "completions was %0d, expected 1", bus.completions);
    $display("All checks passed");
  end
endmodule
