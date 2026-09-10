// A generate block defines a scope (LRM 23.9), so a subroutine it declares is
// named through that block rather than in the module's own namespace: LRM 23.6
// reaches it by hierarchical name from the enclosing module, from a sibling
// block, and from an absolute path. A loop generate elaborates one block per
// iteration (LRM 27.4), so each iteration's subroutine reaches that
// iteration's own state and no other's, and a task declared in a block
// suspends its caller exactly as one declared in the module would.
module Top;
  int from_sibling = 0;

  if (1) begin : cond
    int owned = 5;

    function automatic int Doubled();
      return owned * 2;
    endfunction

    task automatic Bump();
      #1;
      owned = owned + 1;
    endtask
  end

  for (genvar i = 0; i < 3; i++) begin : loop
    int seeded = (i + 1) * 100;

    function automatic int Seeded();
      return seeded;
    endfunction
  end

  if (1) begin : peer
    initial from_sibling = cond.Doubled();
  end

  int from_body = 0;
  int rooted = 0;
  int first = 0;
  int last = 0;
  int resumed_at = 0;

  initial begin
    from_body = cond.Doubled();
    rooted = $root.Top.cond.Doubled();
    first = loop[0].Seeded();
    last = loop[2].Seeded();
    cond.Bump();
    resumed_at = $time;
  end

  final begin
    if (from_body !== 10)
      $fatal(1, "the module body read %0d, expected 10", from_body);
    if (from_sibling !== 10)
      $fatal(1, "a sibling block read %0d, expected 10", from_sibling);
    if (rooted !== 10)
      $fatal(1, "an absolute path read %0d, expected 10", rooted);
    if (first !== 100)
      $fatal(1, "loop[0] reached %0d, expected 100", first);
    if (last !== 300)
      $fatal(1, "loop[2] reached %0d, expected 300", last);
    if (cond.owned !== 6)
      $fatal(1, "the enabled task left %0d, expected 6", cond.owned);
    if (resumed_at !== 1)
      $fatal(1, "the enabler resumed at %0d, expected 1", resumed_at);
    $display("All checks passed");
  end
endmodule
