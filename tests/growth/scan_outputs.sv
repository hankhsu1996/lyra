// A scan of N values, each into its own variable.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.integral constants 1, lir.integral constants 1,
// @each-adds: mir.class constants 1, lir.constants 1
module Top #(parameter int N = 4);
  int [[v{i}|, ]];
  int n;

  initial n = $sscanf("[[{i}| ]]", "[[%d|]]", [[v{i}|, ]]);
endmodule
