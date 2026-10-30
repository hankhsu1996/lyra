// A function body may hold no statements at all, in which case the call yields
// the current value of the implicit variable that carries the return value.
// Never having been assigned, that variable holds its type's default initial
// value: zero for a two-state type and x for a four-state one
// (LRM 13.4, 13.4.1, 6.8).
module Top;
  int two_state;
  logic [7:0] four_state;

  function automatic int nothing(input int n);
  endfunction

  function automatic logic [7:0] unwritten(input logic [7:0] n);
  endfunction

  initial begin
    two_state = 123;
    four_state = 8'h55;
    two_state = nothing(7);
    four_state = unwritten(8'hAA);
  end

  final begin
    if (two_state !== 0) $fatal(1, "two_state was %0d, expected 0", two_state);
    if (four_state !== 8'hxx)
      $fatal(1, "four_state was %h, expected xx", four_state);
    $display("All checks passed");
  end
endmodule
