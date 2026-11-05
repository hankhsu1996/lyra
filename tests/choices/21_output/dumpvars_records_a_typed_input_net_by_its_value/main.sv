// An input port written with a data type and no port kind is a net of the
// default net type (LRM 23.2.2.3), and a dump declares a net by its net type
// (LRM 21.7.2.1 `var_type`). A net of a 2-state integral type is declared so,
// at its width. A net of a real has no net type the format names for it,
// since no net type takes a real (LRM 6.7.1); Lyra declares it by its value,
// `real`, as a variable of that type is.
`timescale 1ns / 1ns
module Leaf (
    input real level,
    input bit [99:0] wide
);
endmodule

module Top;
  real level = 2.5;
  bit [99:0] wide = {4'h9, 96'h1};
  Leaf u (.*);

  initial begin
    $dumpfile("typed_input_net.vcd");
    $dumpvars(1, Top.u);
    #1 $display("All checks passed");
  end
endmodule
