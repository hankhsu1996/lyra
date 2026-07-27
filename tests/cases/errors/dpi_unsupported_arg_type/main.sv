// An SV type outside the DPI-C mapping Lyra wires (LRM 35.5.6, Table H.1) is
// rejected at the declaration rather than mis-marshalled at the call.
// `shortreal` stands for that family; a packed struct and any other unmapped
// type reject through the same classification.
module Top;
  import "DPI-C" function void take(input shortreal f);
  initial take_nothing();
  function void take_nothing();
  endfunction
endmodule
