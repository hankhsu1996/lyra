`timescale 1ns / 1ps
// An export declared inside a generate scope (LRM 27.6). The scope publishes an
// entry that runs against its own instance, while the C name is one
// program-global symbol (LRM 35.4) that resolves against whichever scope the
// foreign call established. The import is declared in that same scope, which is
// what makes the direct call legal (LRM 35.5.3), and the scope is replicated,
// so the one symbol reaches each replica's own state.
module Top;
  for (genvar i = 0; i < 2; i++) begin : g
    int tag = 100 + i;

    export "DPI-C" function read_tag;
    function int read_tag();
      return tag;
    endfunction

    import "DPI-C" context function int call_read();
    initial $display("g%0d=%0d", i, call_read());
  end
endmodule
