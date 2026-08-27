// The members of a soft packed union need not be the same size. The union is
// as wide as the widest member needs, and each member's bits are
// right-justified towards the least significant bits, so a narrower member
// views the low end of the shared vector. Assigning to a narrower member
// leaves the bits above it unaffected (LRM 7.3.1).
module Top;
  typedef union soft packed {
    logic [15:0] wide;
    logic [7:0] narrow;
    logic [3:0] nibble;
  } view_t;

  int union_width;
  logic [15:0] wide_read;
  logic [7:0] narrow_read;
  logic [3:0] nibble_read;
  logic [15:0] after_narrow_write;
  logic [15:0] after_nibble_write;

  initial begin
    view_t u;

    union_width = $bits(u);

    u.wide = 16'hAABB;
    wide_read = u.wide;
    narrow_read = u.narrow;
    nibble_read = u.nibble;

    // The bits above each narrower member are set, so a write that disturbed
    // them would show.
    u.wide = 16'hFF00;
    u.narrow = 8'hAB;
    after_narrow_write = u.wide;
    u.nibble = 4'h5;
    after_nibble_write = u.wide;
  end

  final begin
    if (union_width !== 16)
      $fatal(1, "union_width was %0d, expected 16", union_width);
    if (wide_read !== 16'hAABB)
      $fatal(1, "wide_read was %h, expected aabb", wide_read);
    if (narrow_read !== 8'hBB)
      $fatal(1, "narrow_read was %h, expected bb", narrow_read);
    if (nibble_read !== 4'hB)
      $fatal(1, "nibble_read was %h, expected b", nibble_read);
    if (after_narrow_write !== 16'hFFAB)
      $fatal(1, "after_narrow_write was %h, expected ffab",
             after_narrow_write);
    if (after_nibble_write !== 16'hFFA5)
      $fatal(1, "after_nibble_write was %h, expected ffa5",
             after_nibble_write);
    $display("All checks passed");
  end
endmodule
