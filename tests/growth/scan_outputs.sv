// A scan of N values, each into its own variable.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.constants 2, lir.constants 2
module Top #(parameter int N = 4);
  int [[v{i}|, ]];
  int n;

  initial n = $sscanf("[[{i}| ]]", "[[%d|]]", [[v{i}|, ]]);
endmodule
