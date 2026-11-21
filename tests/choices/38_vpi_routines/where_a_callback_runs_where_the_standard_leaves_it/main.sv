// Where LRM 38.36 leaves a callback's moment to the tool, Lyra's choices. A
// read-write callback runs in Pre-NBA or Post-NBA (LRM Table 4-1); Lyra runs
// it in Post-NBA, after the nonblocking updates, as "after execution of events
// for a specified time" reads. An after-delay callback of no delay asked for
// once the slot's Pre-Active region has run is no error the clause names, so
// Lyra runs it in the slot's Active region. A signal a callback traps is noted
// as it arrives and called back between two region passes, so one raised
// inside a foreign call is called back in the same time step, and before the
// next region is chosen: a process a write of the callback wakes runs ahead of
// a nonblocking update already pending (LRM 4.5).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place_now();
  import "DPI-C" context function int raise_again();
  import "DPI-C" context function int read_write_calls();
  import "DPI-C" context function int read_write_saw();
  import "DPI-C" context function int after_calls();
  import "DPI-C" context function int after_at();
  import "DPI-C" context function int signal_calls();
  import "DPI-C" context function int signal_at();

  int x = 0;
  int y = 0;
  int poked = 0;
  int y_when_poked_again = -1;

  always @(poked) if (poked == 2) y_when_poked_again = y;

  initial begin
    #3;
    x <= 1;
    if (place_now() !== 0) $fatal(1, "placing the callbacks failed");
    #1;
    if (read_write_calls() !== 1 || read_write_saw() !== 1)
      $fatal(1, "a read-write callback ran %0d times, saw %0d",
             read_write_calls(), read_write_saw());
    if (after_calls() !== 1 || after_at() !== 3)
      $fatal(1, "an after-delay callback of no delay ran %0d times, at %0d",
             after_calls(), after_at());
    if (signal_calls() !== 1 || signal_at() !== 3)
      $fatal(1, "a signal was called back %0d times, at %0d", signal_calls(),
             signal_at());
    y <= 1;
    void'(raise_again());
    #1;
    if (signal_calls() !== 2 || y_when_poked_again !== 0)
      $fatal(1, "a signal was called back %0d times, its write seen with y %0d",
             signal_calls(), y_when_poked_again);
    $display("All checks passed");
  end
endmodule
