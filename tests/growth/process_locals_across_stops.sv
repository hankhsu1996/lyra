// N automatic variables alive across N stops: every stop's way out ends the same
// variables, so a step adds one variable and one stop.
// @each-adds: bytes,
// @each-adds: mir.captures 2, mir.closures 1, mir.types 3,
// @each-adds: lir.captures 2, lir.closures 1, lir.functions 2,
// @each-adds: lir.type steps 1, lir.types 4
module Top #(parameter int N = 4);
  bit clk;
  int c;

  initial begin
    [[automatic string s{i} = "x";| ]]
    [[@(posedge clk);| ]]
    [[c = c + s{i}.len();| ]]
  end
endmodule
