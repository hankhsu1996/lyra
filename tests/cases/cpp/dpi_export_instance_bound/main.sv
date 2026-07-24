`timescale 1ns / 1ps
// LRM 35.5.3 instance-bound export dispatch: an exported subroutine runs against
// the instance the call-chain context names, not one hardcoded instance. `Sub` is
// instantiated twice; a single exported `read_id` wrapper reaches whichever
// instance is the current DPI scope. The context import `read_at` sets the scope
// to a named instance via svSetScope before calling the export, so the same
// wrapper returns each instance's own `id` -- proving the receiver is the current
// scope, which a single-top-instance recovery could not express.
module Sub #(parameter int ID = 0);
  int id;
  export "DPI-C" function read_id;
  function int read_id();
    return id;
  endfunction
  initial id = ID;
endmodule

module Top;
  Sub #(.ID(10)) m0();
  Sub #(.ID(20)) m1();

  import "DPI-C" context function int read_at(input string path);
  initial begin
    #1;
    $display("m0=%0d", read_at("Top.m0"));
    $display("m1=%0d", read_at("Top.m1"));
  end
endmodule
