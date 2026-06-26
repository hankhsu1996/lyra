module Top;
  // Unpacked array constant -- the localparam form ibex_alu's shuffle masks use.
  localparam logic [31:0] Tbl [4] = '{32'd10, 32'd20, 32'd30, 32'd40};

  // Continuous assign reading the constant array with a constant element index.
  int ca_first;
  int ca_last;
  assign ca_first = Tbl[0];
  assign ca_last = Tbl[3];

  // Generate loop unrolling an element-select on the constant array -- the exact
  // form that first surfaced the gap in ibex_alu.
  logic [31:0] bumped [4];
  for (genvar i = 0; i < 4; i++) begin : gen_bump
    assign bumped[i] = Tbl[i] + 32'd1;
  end
  int ca_gen0;
  assign ca_gen0 = bumped[0];

  // Procedural element-select reading the constant array.
  int sum;
  initial begin
    sum = Tbl[1] + Tbl[2];
  end
endmodule
