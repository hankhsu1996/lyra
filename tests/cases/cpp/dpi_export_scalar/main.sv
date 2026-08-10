// LRM 35.5 DPI-C export: a foreign C function, called from a running
// simulation, calls back an exported SV function. `round_trip` is imported and
// its C body calls the exported `sv_double`, so a result of 42 (21 doubled)
// proves the whole import -> export chain ran: a no-op import would read 0, and
// an import that returned its argument without calling back would read 21.
//
// The import is `context`, so calling back an export is legal (LRM 35.5.3); the
// export's receiver is the calling instance -- here the single top instance --
// recovered from the call chain's current DPI scope.
module Top;
  import "DPI-C" context function int round_trip(input int x);
  export "DPI-C" function sv_double;
  function int sv_double(input int x);
    return x * 2;
  endfunction

  // An export declared inside a generate block belongs to that block's scope
  // object rather than to the module, so its entry point recovers that scope as
  // the receiver. The import that calls back is declared in the same block, so
  // the scope the call chain establishes is the one the export needs
  // (LRM 35.5.3). The entry point is a program-global C symbol either way
  // (LRM 35.7), so where the subroutine it dispatches into was declared changes
  // nothing the foreign caller sees.
  if (1) begin : gen_helper
    import "DPI-C" context function int gen_round_trip(input int x);
    export "DPI-C" function sv_triple;
    function int sv_triple(input int x);
      return x * 3;
    endfunction
    initial $display("g=%0d", gen_round_trip(7));
  end

  int r;
  initial begin
    r = round_trip(21);
    $display("r=%0d", r);
  end
endmodule
