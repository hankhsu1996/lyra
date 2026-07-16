// A DPI-C export whose argument is not a by-value input is not yet supported:
// an output argument crosses the boundary by pointer, which the export
// marshaling does not yet handle (LRM 35.5.5).
module Top;
  export "DPI-C" function set_val;

  function automatic int set_val(output int o);
    o = 1;
    return 42;
  endfunction
endmodule
