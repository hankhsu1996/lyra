module Top;
  parameter int unsigned BYTES = 8;

  // LRM 20.8.1: $clog2 returns ceil(log2(arg)); arg is unsigned and $clog2(0)
  // is 0. These exercise the constant-argument path (folded downstream, never
  // in lowering): an exact power of two, a value forcing the ceiling, the zero
  // special case, and a wider value.
  int c_const;
  int c_zero;
  int c_seven;
  int c_sixteen;

  initial begin
    c_const = $clog2(BYTES);
    c_zero = $clog2(0);
    c_seven = $clog2(7);
    c_sixteen = $clog2(16);
  end

  // Genvar-dependent argument: `i` is a runtime induction value, so the
  // argument is a runtime expression and $clog2 must lower to a runtime call,
  // not a constant. Lane i holds $clog2((2**(i+1)) - 1): 0, 2, 3, 4.
  logic [31:0] genvar_out;
  for (genvar i = 0; i < 4; i++) begin : gen
    assign genvar_out[8 * i +: 8] = $clog2((1 << (i + 1)) - 1);
  end
endmodule
