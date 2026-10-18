// LRM 10.6.2: a `force` may name a constant bit-select or a constant
// part-select of a vector net, or a concatenation of such selects, and then
// overrides every driver of those bits alone until a `release` of them. Every
// other bit of the net goes on showing what its drivers resolve to, and a
// released bit is immediately assigned the value its drivers determine. The
// right-hand side is a continuous assignment while the force is in effect
// (LRM 10.6), and a second force of the same bits replaces the first.
module Top;
  logic [7:0] d = 8'h00;
  logic b = 1'b0;
  wire [7:0] n;
  wire [3:0] m;

  assign n = d;
  assign m = 4'h0;

  // A generate block's bit is a constant select in each block (LRM 27.4).
  for (genvar bit_at = 6; bit_at < 8; bit_at++) begin : g
    initial begin
      #20;
      force n[bit_at] = 1'b1;
      #2;
      release n[bit_at];
    end
  end

  initial begin
    #1;
    force n[3] = 1'b1;
    #1;
    if (n !== 8'h08) $fatal(1, "a forced bit read %h, expected 08", n);

    d = 8'hf0;
    #1;
    if (n !== 8'hf8)
      $fatal(1, "the other bits did not follow the driver: %h, expected f8", n);

    force n[7:4] = 4'h5;
    #1;
    if (n !== 8'h58)
      $fatal(1, "a forced part-select read %h, expected 58", n);

    d = 8'h00;
    #1;
    if (n !== 8'h58)
      $fatal(1, "a driver moved forced bits: %h, expected 58", n);

    release n[3];
    #1;
    if (n !== 8'h50)
      $fatal(1, "a released bit read %h, expected its driver's 50", n);

    release n[7:4];
    #1;
    if (n !== 8'h00)
      $fatal(1, "a released part-select read %h, expected 00", n);

    force n[1:0] = {b, b};
    #1;
    if (n !== 8'h00) $fatal(1, "a forced expression read %h, expected 00", n);
    b = 1'b1;
    #1;
    if (n !== 8'h03)
      $fatal(1, "a forced expression did not follow its operand: %h", n);
    force n[1:0] = 2'b10;
    b = 1'b0;
    #1;
    if (n !== 8'h02)
      $fatal(1, "a second force did not replace the first: %h, expected 02", n);
    release n[1:0];

    force {n[5], m[2]} = 2'b11;
    #1;
    if (n !== 8'h20 || m !== 4'h4)
      $fatal(1, "a forced concatenation read %h %h, expected 20 4", n, m);
    release {n[5], m[2]};
    #1;
    if (n !== 8'h00 || m !== 4'h0)
      $fatal(1, "a released concatenation read %h %h, expected 00 0", n, m);

    #9;
    if (n !== 8'hc0)
      $fatal(1, "forces from two blocks read %h, expected c0", n);
    #2;
    if (n !== 8'h00)
      $fatal(1, "releases from two blocks read %h, expected 00", n);

    $display("All checks passed");
  end
endmodule
