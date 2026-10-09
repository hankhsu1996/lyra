// Each block instance of a loop is its own scope (LRM 27.4), so a subroutine a
// block declares is that instance's: enabled by a hierarchical name selecting
// one instance (LRM 23.6), it runs in that instance and reaches what that
// instance's own text names. Here the text of every block names a different
// neighbour, by an index computed from the block's own, so no two instances'
// subroutines do the same thing; and they are enabled from another module, by
// a name that steps through the instance holding the loop.
module Ring;
  for (genvar i = 0; i < 3; i++) begin : ring
    int own = -1;

    function automatic int NextOwn();
      return ring[(i + 1) % 3].own;
    endfunction

    task automatic PassOn();
      #1;
      ring[(i + 1) % 3].own = own + 1;
    endtask

    // The index is an implicit localparam of the block instance (LRM 27.4),
    // so it may size a declaration of the subroutine, a different one in
    // every instance.
    function automatic int Wide();
      logic [i:0] sized;
      return $bits(sized);
    endfunction

    initial own = (i + 1) * 100;
  end
endmodule

module Top;
  Ring r ();

  int after_zero = -1;
  int after_one = -1;
  int after_two = -1;
  int wide [3] = '{-1, -1, -1};

  initial begin
    wide[0] = r.ring[0].Wide();
    wide[1] = r.ring[1].Wide();
    wide[2] = r.ring[2].Wide();
    #1;
    after_zero = r.ring[0].NextOwn();
    after_one = r.ring[1].NextOwn();
    after_two = r.ring[2].NextOwn();
    r.ring[2].PassOn();
  end

  final begin
    for (int k = 0; k < 3; k++) begin
      if (wide[k] !== k + 1)
        $fatal(1, "ring[%0d] sized %0d bits, expected %0d", k, wide[k], k + 1);
    end
    if (after_zero !== 200)
      $fatal(1, "ring[0] read its neighbour as %0d, expected 200", after_zero);
    if (after_one !== 300)
      $fatal(1, "ring[1] read its neighbour as %0d, expected 300", after_one);
    if (after_two !== 100)
      $fatal(1, "ring[2] read its neighbour as %0d, expected 100", after_two);
    if (r.ring[0].own !== 301)
      $fatal(
          1, "ring[2] passed on %0d to ring[0], expected 301", r.ring[0].own);
    if (r.ring[1].own !== 200)
      $fatal(1, "ring[1] holds %0d, expected 200", r.ring[1].own);
    $display("All checks passed");
  end
endmodule
