// The foreign side reaches an element through the one-, two-, and three-index
// entries of LRM Annex H.12.3; a deeper array would need the variable-argument
// forms.
module Top;
  import "DPI-C" function void take(input int m[][][][]);
  initial take_nothing();
  function void take_nothing();
  endfunction
endmodule
