// A function of N inputs, called with N arguments.
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  function automatic int f([[input int x{i}|, ]]);
    return x0 + x{last};
  endfunction

  int r;

  initial r = f([[{i}|, ]]);
endmodule
