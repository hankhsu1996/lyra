// Because a ref argument shares the caller's variable rather than a copy of
// it, a write through the formal is visible outside the subroutine as soon as
// it happens and not at the return: the variable read by its own name from
// inside the body already carries the new value, and the write is a change
// that an event control on that variable observes (LRM 13.5.2, 9.4.2).
module Top;
  int g;
  int seen_inside;
  int woke;

  function automatic void poke(ref int x);
    x = x + 1;
    seen_inside = g;
  endfunction

  initial begin
    @(g);
    woke = 1;
  end

  initial begin
    #1;
    poke(g);
  end

  final begin
    if (g !== 1) $fatal(1, "g was %0d, expected 1", g);
    if (seen_inside !== 1)
      $fatal(1, "seen_inside was %0d, expected 1", seen_inside);
    if (woke !== 1) $fatal(1, "woke was %0d, expected 1", woke);
    $display("All checks passed");
  end
endmodule
