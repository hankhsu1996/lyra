// Multi-driver `wire` / `tri` resolution (LRM 6.6.1, Table 6-2): a net's value
// is the resolution of all its drivers, not the last write. Agreement passes
// through, a 0/1 conflict yields `x`, and a driver at high-impedance defers to
// the others because `z` is the resolution identity. Both same-scope multiple
// `assign`s and a driver arriving across a port alongside a local driver are
// covered; the N=1 single-driver case lives in `nets_wire_single_driver`.
//
// An assignment naming only part of a net drives only that part, which is the
// same rule read at bit granularity: the driver contributes high-impedance
// everywhere it does not drive, so disjoint partial drivers compose and
// overlapping ones conflict exactly as whole-net drivers do.
module Sink(output wire [7:0] o);
  assign o = 8'hA5;
endmodule

module Top;
  logic [7:0] a, b;
  wire  [7:0] agree;
  wire  [7:0] conflict;
  wire  [7:0] with_z;
  wire  [7:0] ported;
  wire  [7:0] halves;
  wire  [7:0] overlap;
  wire        elements[0:1];

  assign agree = a;
  assign agree = b;

  assign conflict = 8'h0F;
  assign conflict = 8'hFF;

  assign with_z = 8'hzz;
  assign with_z = 8'h3C;

  assign ported = 8'h5A;
  Sink u(.o(ported));

  assign halves[3:0] = a[3:0];
  assign halves[7:4] = 4'h9;

  assign overlap[7:4] = 4'h9;
  assign overlap[5:2] = 4'h9;

  assign elements[0] = 1'b1;
  assign elements[1] = 1'b0;

  initial begin
    a = 8'hC3;
    b = 8'hC3;
    #1;
    $display("agree=%b", agree);
    $display("conflict=%b", conflict);
    $display("with_z=%b", with_z);
    $display("ported=%b", ported);
    $display("halves=%b", halves);
    $display("overlap=%b", overlap);
    $display("elements=%b%b", elements[0], elements[1]);
  end
endmodule
