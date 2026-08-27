// An argument may be bound to its formal by name rather than by position, in
// which case the order the names are written in does not matter. A call may
// mix the two forms so long as every positional argument comes before the
// first named one (LRM 13.5.4).
module Top;
  int named;
  int reordered;
  int mixed;

  function automatic int sub(int a, int b);
    return a - b;
  endfunction

  initial begin
    named = sub(.a(20), .b(3));
    reordered = sub(.b(3), .a(20));
    mixed = sub(20, .b(3));
  end

  final begin
    if (named !== 17) $fatal(1, "named was %0d, expected 17", named);
    if (reordered !== 17)
      $fatal(1, "reordered was %0d, expected 17", reordered);
    if (mixed !== 17) $fatal(1, "mixed was %0d, expected 17", mixed);
    $display("All checks passed");
  end
endmodule
