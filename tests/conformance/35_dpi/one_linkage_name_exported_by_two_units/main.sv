// One linkage name may be exported from more than one scope, so long as the
// type signatures agree; what LRM 35.4 forbids is a second export of it within
// one scope. Every declaring scope therefore contributes a definition of the
// one global symbol the name has, and whatever assembles the program keeps a
// single one. A call still runs the subroutine of the scope the DPI call chain
// currently holds (LRM 35.5.3), so the two scopes answer differently through
// the one symbol.
module Alpha;
  int factor;
  int seen = -1;

  export "DPI-C" function shared_scale;
  function int shared_scale(int x);
    return x * factor;
  endfunction

  // Declared in the same scope as the export it reaches, so the chain this
  // import starts already holds that scope (LRM 35.5.3).
  import "DPI-C" context function int call_shared(input int x);

  initial begin
    factor = 3;
    seen   = call_shared(7);
  end
endmodule

module Beta;
  int factor;
  int seen = -1;

  export "DPI-C" function shared_scale;
  function int shared_scale(int x);
    return x * factor;
  endfunction

  import "DPI-C" context function int call_shared(input int x);

  initial begin
    factor = 5;
    seen   = call_shared(7);
  end
endmodule

module Top;
  Alpha a ();
  Beta b ();

  final begin
    // Two answers that differ are what separates one surviving definition
    // serving both scopes from one scope standing in for the other.
    if (a.seen !== 21) $fatal(1, "Alpha answered %0d, expected 21", a.seen);
    if (b.seen !== 35) $fatal(1, "Beta answered %0d, expected 35", b.seen);
    $display("All checks passed");
  end
endmodule
