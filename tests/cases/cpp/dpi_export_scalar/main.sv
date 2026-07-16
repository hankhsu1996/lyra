// LRM 35.5 DPI-C export: a foreign C function, called from a running
// simulation, calls back an exported SV function. `round_trip` is imported and
// its C body calls the exported `sv_double`, so a result of 42 (21 doubled)
// proves the whole import -> export chain ran: a no-op import would read 0, and
// an import that returned its argument without calling back would read 21.
//
// `sv_double` reads no instance state, so its receiver is the sole top
// instance -- the receiver-less / singleton export shape.
module Top;
  import "DPI-C" function int round_trip(input int x);
  export "DPI-C" function sv_double;
  function int sv_double(input int x);
    return x * 2;
  endfunction

  int r;
  initial begin
    r = round_trip(21);
    $display("r=%0d", r);
  end
endmodule
