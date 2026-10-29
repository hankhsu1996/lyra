// $dumpvars counts levels of the hierarchy below each module instance it names
// (LRM 21.7.1.2), and a dump declares each scope as one of begin, fork,
// function, module or task (LRM 21.7.2.1). Neither says where a generate block
// falls: whether it is a level of its own, and which of those kinds it is
// declared as. Lyra takes a generate block as part of the instance holding it,
// so a level count of 1 reaches the variables a block of the named instance
// declares, and declares the block as a begin scope.
`timescale 1ns / 1ns
module Mid;
  logic m = 1'b0;
  if (1) begin : gen
    logic g = 1'b1;
  end
endmodule

module Top;
  Mid u ();

  initial begin
    $dumpfile("generate_block.vcd");
    $dumpvars(1, Top.u);
    #1 $display("All checks passed");
  end
endmodule
