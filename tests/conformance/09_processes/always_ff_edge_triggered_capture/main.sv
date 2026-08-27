// An always_ff procedure carries one and only one event control (LRM 9.2.2.4),
// so its body runs when that event occurs and at no other moment. An edge is
// detected in one direction alone, so a procedure controlled by a posedge takes
// no notice of the falling transition and one controlled by a negedge takes no
// notice of the rising transition, and a change to the captured input between
// two edges is held until the next edge arrives (LRM 9.4.2).
module Top;
  bit clk;
  bit en;
  int d;
  int q_rise;
  int q_fall;
  int q_enabled;

  int rise_between_edges;
  int fall_between_edges;

  always_ff @(posedge clk) q_rise <= d;
  always_ff @(negedge clk) q_fall <= d;

  always_ff @(posedge clk) begin
    if (en) q_enabled <= d;
  end

  initial begin
    en = 1;
    d = 42;
    #5 clk = 1;
    #3 d = 43;
    #2 clk = 0;
    #2 rise_between_edges = q_rise;
    #1 d = 99;
    #2 clk = 1;
    #2 fall_between_edges = q_fall;
    #3 clk = 0;
    #1 en = 0;
    d = 7;
    #4 clk = 1;
    #5 clk = 0;
    #1;
  end

  final begin
    if (rise_between_edges !== 42)
      $fatal(1, "rise_between_edges was %0d, expected 42", rise_between_edges);
    if (fall_between_edges !== 43)
      $fatal(1, "fall_between_edges was %0d, expected 43", fall_between_edges);
    if (q_rise !== 7) $fatal(1, "q_rise was %0d, expected 7", q_rise);
    if (q_fall !== 7) $fatal(1, "q_fall was %0d, expected 7", q_fall);
    if (q_enabled !== 99)
      $fatal(1, "q_enabled was %0d, expected 99", q_enabled);
    $display("All checks passed");
  end
endmodule
