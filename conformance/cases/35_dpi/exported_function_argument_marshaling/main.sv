// An exported function is held to the same restrictions on argument types and
// results as an imported one, so its formals cross in the same shapes: a small
// value in a register, an output or inout scalar through a pointer to that
// register type, a packed value through a pointer to a canonical buffer, and a
// result restricted to a small value (LRM 35.5.5, 35.5.6, 35.7, Annex H.8.3,
// H.8.4, H.8.8, H.8.9). The foreign caller allocates whatever an argument
// passed by reference needs (LRM Annex H.8.5). An inout the function changes
// comes back changed even where the new value compares equal to the old, as a
// real's negated zero does. A `shortreal` crosses as C's `float` both ways,
// and a packed structure as the packed array it is equivalent to (Table H.1,
// Annex H.7.4).
module Top;
  typedef struct packed {
    bit [7:0] tens;
    bit [7:0] units;
  } digits_t;
  typedef struct packed {
    logic [3:0] high;
    logic [3:0] low;
  } halves_t;
  typedef enum int {
    kOff = 7,
    kOn = 11
  } switch_t;

  import "DPI-C" context function int drive();

  export "DPI-C" function scale_pair;
  export "DPI-C" function accumulate;
  export "DPI-C" function fill_wide;
  export "DPI-C" function weigh_wide;
  export "DPI-C" function make_pattern;
  export "DPI-C" function classify;
  export "DPI-C" function step_logic;
  export "DPI-C" function negate;
  export "DPI-C" function quarter;
  export "DPI-C" function read_digits;
  export "DPI-C" function swap_halves;
  export "DPI-C" function switch_code;

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

  function void negate(inout real r);
    r = -r;
  endfunction

  function shortreal quarter(input shortreal s, output shortreal rest);
    rest = s - (s / 4.0);
    return s / 4.0;
  endfunction

  function int read_digits(input digits_t d);
    return (d.tens * 10) + d.units;
  endfunction

  function void swap_halves(inout halves_t h);
    h = '{high: h.low, low: h.high};
  endfunction

  function int switch_code(input switch_t s);
    return s == kOn ? 1 : 2;
  endfunction

  int verdict;

  initial verdict = drive();

  final begin
    // One bit per crossing the foreign side checked, so a failure names the
    // argument shape that did not survive.
    if (verdict !== 32767)
      $fatal(1, "the foreign side reported %0d of 32767 crossings", verdict);
    $display("All checks passed");
  end
endmodule
