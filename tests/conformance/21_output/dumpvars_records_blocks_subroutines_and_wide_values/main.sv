// A dump records the static variables of named blocks, tasks and functions in
// scopes of their own, declared as begin, fork, task and function (LRM
// 21.7.2.1 `scope_type`), and records a vector of any width, past the 64 bits
// a machine word holds as well as within them: a variable, a net and a port
// (LRM 21.7.1.2, 21.7.2.2). The subroutines are automatic, so the only
// variables in their scopes are the static ones they declare, and the port is
// declared a net. The dump the run writes is held to the one in expected/,
// read as the standard reads a dump.
`timescale 1ns / 1ns
module Leaf (input wire logic [99:0] in);
endmodule

module Top;
  logic [99:0] wide = 100'h1;
  wire [99:0] wide_net = wide;
  Leaf u (.in(wide));

  task automatic bump;
    static logic [3:0] count = 4'h0;
    count = count + 1;
  endtask

  function automatic void twice;
    static int calls = 0;
    calls = calls + 1;
  endfunction

  initial begin : body
    static logic [3:0] held = 4'h9;
    $dumpfile("blocks.vcd");
    $dumpvars;
    fork : side
      static logic forked = 1'b1;
    join
    #1 bump;
    twice;
    held = 4'h6;
    wide = {4'ha, 96'h0} | 100'h5;
    #1 $display("All checks passed");
  end
endmodule
