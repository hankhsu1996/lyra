// LRM Annex H.11.4 requires an unpacked array that is not an open array to
// have C-compiler layout, which a SystemVerilog value does not have.
module Top;
  import "DPI-C" function void take(input byte s[1:4]);
  initial take_nothing();
  function void take_nothing();
  endfunction
endmodule
