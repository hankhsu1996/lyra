// LRM 35.5 DPI-C export marshaling of 4-state values across the boundary: a
// foreign C function calls back exported SV functions carrying `logic` scalars
// and `logic` vectors, whose unknown (X/Z) bits cross as the canonical
// svLogicVecVal / svLogic bval plane (Annex H.10.1). The imported `drive`
// validates the planes both ways and folds the outcome into one integer.
//
// Coverage in one module: a `logic` vector `output` carrying X and Z, a `logic`
// vector `input` counting unknown bits, and a scalar `logic` input / output
// round-tripping an X.
module Top;
  import "DPI-C" context function int drive();

  export "DPI-C" function make_zx;
  export "DPI-C" function count_x;
  export "DPI-C" function pass_logic;

  function void make_zx(output logic [7:0] v);
    v = 8'b1x0z_1x0z;
  endfunction

  function int count_x(input logic [7:0] v);
    int c;
    c = 0;
    for (int i = 0; i < 8; i++) begin
      if (v[i] === 1'bx || v[i] === 1'bz) begin
        c = c + 1;
      end
    end
    return c;
  endfunction

  function void pass_logic(input logic a, output logic b);
    b = a;
  endfunction

  int r;
  initial begin
    r = drive();
    $display("r=%0d", r);
  end
endmodule
