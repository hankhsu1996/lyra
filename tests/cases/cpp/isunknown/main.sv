module Top;
  logic [3:0] a;
  bit r_known;
  bit r_x;
  bit r_z;

  logic [3:0] bus_known;
  logic [3:0] bus_xz;
  bit ca_known;
  bit ca_xz;

  // LRM 20.9 $isunknown in a continuous assignment (simulation-time value
  // expression driving a net).
  assign ca_known = $isunknown(bus_known);
  assign ca_xz = $isunknown(bus_xz);

  // LRM 20.9 $isunknown in procedural code over known, x, and z operands.
  initial begin
    a = 4'b0101;
    r_known = $isunknown(a);
    a = 4'b01x1;
    r_x = $isunknown(a);
    a = 4'b01z1;
    r_z = $isunknown(a);
    bus_known = 4'b1010;
    bus_xz = 4'b1z10;
  end
endmodule
