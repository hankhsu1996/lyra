// An exported function is held to the same restrictions on argument types and
// results as an imported one, so its formals cross in the same shapes: a small
// value in a register, an output or inout scalar through a pointer to that
// register type, a packed value through a pointer to a canonical buffer, and a
// result restricted to a small value (LRM 35.5.5, 35.5.6, 35.7, Annex H.8.3,
// H.8.4, H.8.8, H.8.9). The foreign caller allocates whatever an argument
// passed by reference needs (LRM Annex H.8.5).
module Top;
  import "DPI-C" context function int drive();

  export "DPI-C" function scale_pair;
  export "DPI-C" function accumulate;
  export "DPI-C" function fill_wide;
  export "DPI-C" function weigh_wide;
  export "DPI-C" function make_pattern;
  export "DPI-C" function classify;
  export "DPI-C" function step_logic;

  function void scale_pair(input int seed, output int lo, output int hi);
    lo = seed * 2;
    hi = seed * 3;
  endfunction

  function void accumulate(inout int acc);
    acc = (acc * 10) + 3;
  endfunction

  function void fill_wide(input int seed, output bit [127:0] w);
    w = 128'd0;
    w[31:0] = seed;
    w[63:32] = seed + 1;
    w[95:64] = seed + 2;
    w[127:96] = seed + 3;
  endfunction

  function int weigh_wide(input bit [127:0] w);
    return w[31:0] + (2 * w[63:32]) + (3 * w[95:64]) + (4 * w[127:96]);
  endfunction

  function void make_pattern(input int seed, output logic [7:0] v);
    if (seed == 1) v = 8'b1x0z_1x0z;
    else v = 8'b0z1x_0z1x;
  endfunction

  // A base-four reading of the whole value, one digit per bit, so no two
  // eight-bit four-state values share an answer.
  function int classify(input logic [7:0] v);
    int code;
    code = 0;
    for (int i = 7; i >= 0; i--) begin
      if (v[i] === 1'b0) code = code * 4;
      else if (v[i] === 1'b1) code = (code * 4) + 1;
      else if (v[i] === 1'bz) code = (code * 4) + 2;
      else code = (code * 4) + 3;
    end
    return code;
  endfunction

  function void step_logic(input logic a, output logic b);
    if (a === 1'b0) b = 1'b1;
    else if (a === 1'b1) b = 1'bz;
    else if (a === 1'bz) b = 1'bx;
    else b = 1'b0;
  endfunction

  int verdict;

  initial verdict = drive();

  final begin
    // One bit per crossing the foreign side checked, so a failure names the
    // argument shape that did not survive.
    if (verdict !== 511)
      $fatal(1, "the foreign side reported %0d of 511 crossings", verdict);
    $display("All checks passed");
  end
endmodule
