// A string formatted from N items.
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  string s;

  initial s = $sformatf("[[%0d|]]", [[{i}|, ]]);
endmodule
