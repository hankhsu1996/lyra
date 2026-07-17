// Multi-driver `wire` / `tri` resolution (LRM 6.6.1, Table 6-2): a net's value
// is the resolution of all its drivers, not the last write. Agreement passes
// through, a 0/1 conflict yields `x`, and a driver at high-impedance defers to
// the others because `z` is the resolution identity. Both same-scope multiple
// `assign`s and a driver arriving across a port alongside a local driver are
// covered; the N=1 single-driver case lives in `nets_wire_single_driver`.
module Sink(output wire [7:0] o);
  assign o = 8'hA5;
endmodule

module Top;
  logic [7:0] a, b;
  wire  [7:0] agree;
  wire  [7:0] conflict;
  wire  [7:0] with_z;
  wire  [7:0] ported;

  assign agree = a;
  assign agree = b;

  assign conflict = 8'h0F;
  assign conflict = 8'hFF;

  assign with_z = 8'hzz;
  assign with_z = 8'h3C;

  assign ported = 8'h5A;
  Sink u(.o(ported));

  initial begin
    a = 8'hC3;
    b = 8'hC3;
    #1;
    $display("agree=%b", agree);
    $display("conflict=%b", conflict);
    $display("with_z=%b", with_z);
    $display("ported=%b", ported);
  end
endmodule
