module Top;
  int g;
  int woke;

  function automatic void poke(ref int x);
    x = x + 1;
  endfunction

  // Waits for any change to the observable variable g. This only fires if the
  // ref write inside poke routes through g's update-event path (LRM 4.3).
  initial begin
    @(g);
    woke = 1;
  end

  // The #1 lets the waiter above subscribe before g changes, so the wake is
  // deterministic rather than racing the subscription.
  initial begin
    #1;
    poke(g);
  end
endmodule
