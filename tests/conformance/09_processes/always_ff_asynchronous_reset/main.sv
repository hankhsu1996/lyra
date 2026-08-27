// An event control may name several events joined by or, and the occurrence of
// any one of them triggers the procedure (LRM 9.4.2.1); an always_ff carries
// exactly one such control, however many edges it lists (LRM 9.2.2.4). A
// control naming a clock's rising edge and a reset's falling edge therefore
// runs the body the moment reset falls, with no clock edge involved at all,
// while the reset's own rising edge is not one of the listed events and runs
// nothing.
module Top;
  bit clk;
  bit rst_n;
  int q;

  int before_reset;
  int after_reset;
  int after_reset_release;

  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) q <= 0;
    else q <= q + 1;
  end

  initial begin
    rst_n = 1;
    #5 clk = 1;
    #5 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
    #2 before_reset = q;
    #1 rst_n = 0;
    #2 after_reset = q;
    #1 rst_n = 1;
    #2 after_reset_release = q;
    #2 clk = 1;
    #5 clk = 0;
    #5 clk = 1;
    #5 clk = 0;
    #1;
  end

  final begin
    if (before_reset !== 2)
      $fatal(1, "before_reset was %0d, expected 2", before_reset);
    if (after_reset !== 0)
      $fatal(1, "after_reset was %0d, expected 0", after_reset);
    if (after_reset_release !== 0)
      $fatal(1, "after_reset_release was %0d, expected 0", after_reset_release);
    if (q !== 2) $fatal(1, "q was %0d, expected 2", q);
    $display("All checks passed");
  end
endmodule
