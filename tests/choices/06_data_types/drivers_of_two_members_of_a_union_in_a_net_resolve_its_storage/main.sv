// A net's data type may be an unpacked union (LRM 6.7.1), whose members are
// one storage, with no representation the standard requires (LRM 7.3), and a
// union may be written through one member and read through another (LRM
// 7.3.2). What drivers through two of its members give the net, and what a
// whole union value gives beside a driver of a member, is therefore Lyra's to
// say. Lyra holds an untagged union as the bits of its widest member, every
// member a view of them from the first bit, so each driver contributes to the
// bits it reaches through its member, and each bit resolves as any net's does
// (LRM 6.6.7, Table 6-2): a bit one driver reaches shows it, a bit two
// disagree on is x, and a bit none reaches is z.
module Top;
  typedef union { logic [3:0] narrow; logic [15:0] wide; } overlay_t;
  logic [3:0] a;
  logic [7:0] c;
  overlay_t v;

  wire overlay_t apart;
  assign apart.narrow = a;
  assign apart.wide[15:8] = c;

  wire overlay_t overlapping;
  assign overlapping.narrow = 4'b1100;
  assign overlapping.wide[3:0] = 4'b1010;

  wire overlay_t beside;
  assign beside = v;
  assign beside.narrow = 4'h0;

  initial begin
    a = 4'ha;
    c = 8'h5c;
    v.wide = 16'h12ff;
    #1;
    if (apart.wide !== 16'b0101_1100_zzzz_1010)
      $fatal(1, "drivers of two members apart: %b", apart.wide);
    if (overlapping.narrow !== 4'b1xx0)
      $fatal(1, "drivers of two members over one bit: %b", overlapping.narrow);
    if (beside.wide !== 16'b0001_0010_1111_xxxx)
      $fatal(1, "a whole union beside a member's driver: %b", beside.wide);
    $display("All checks passed");
  end
endmodule
