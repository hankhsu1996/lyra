// LRM 35.5 DPI-C export with argument directions beyond a by-value input: a
// foreign C function, called from a running simulation, calls back exported SV
// functions whose formals carry `output` / `inout` scalars and by-pointer
// packed vectors. The imported `drive` exercises every direction and folds the
// results into one integer the SV side asserts, so a marshaling defect in any
// direction moves the total.
//
// Coverage in one module: two scalar `output` formals (a void export), a scalar
// `inout`, a multi-word 2-state vector `output`, and a multi-word 2-state
// vector `input` feeding a non-void return.
module Top;
  import "DPI-C" function int drive();

  export "DPI-C" function get_pair;
  export "DPI-C" function accumulate;
  export "DPI-C" function fill_wide;
  export "DPI-C" function sum_wide;

  function void get_pair(output int lo, output int hi);
    lo = 3;
    hi = 7;
  endfunction

  function void accumulate(inout int acc);
    acc = acc + 100;
  endfunction

  function void fill_wide(output bit [127:0] w);
    w = 128'h00000004_00000003_00000002_00000001;
  endfunction

  function int sum_wide(input bit [127:0] w);
    return w[31:0] + w[127:96];
  endfunction

  int r;
  initial begin
    r = drive();
    $display("r=%0d", r);
  end
endmodule
