// @reports: violation
//
// A variable's initializer runs before any procedure (LRM 10.5) and may call a
// function of another instance through a hierarchical name. A unique-case in
// that function with no item matching is a violation and is reported (LRM
// 12.5.3), whichever instance's initializer made the call. With no process
// running, the report belongs to no deferred report queue and nothing flushes
// it (LRM 12.4.2.1).
module Child;
  function automatic int pick(int a);
    unique case (a)
      0: return 10;
      1: return 11;
    endcase
    return 99;
  endfunction
endmodule

module Top;
  int x = c.pick(5);

  Child c ();

  initial begin
    if (x !== 99) $fatal(1, "x was %0d, expected 99", x);
    $display("Ready for the report");
    #1 $display("All checks passed");
  end
endmodule
