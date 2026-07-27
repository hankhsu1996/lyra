// LRM Annex H.7.3 puts an open array of a `real` element in C-compatible
// representation, which a SystemVerilog value does not have.
module Top;
  import "DPI-C" function void take(input real r[]);
  initial take_nothing();
  function void take_nothing();
  endfunction
endmodule
