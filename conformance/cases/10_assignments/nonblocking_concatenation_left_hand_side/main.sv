// A nonblocking assignment whose left-hand side is a concatenation schedules
// every member of it and updates none of them until the end of the time step,
// so each member still reads its old value for the rest of the step and they
// all take their share of the distributed value together (LRM 10.4.2,
// Table 10-1). The concatenation is one left-hand side, so a timing control on
// such an assignment names the slot every member's share lands in, exactly as
// it does for a single variable (LRM 9.4.5).
module Top;
  logic [7:0] high, mid, low;
  logic [7:0] high_before, mid_before, low_before;

  logic [7:0] delayed_high, delayed_low;
  logic [7:0] delayed_high_midway;
  logic [7:0] edged_high, edged_low;
  logic [7:0] edged_high_midway;

  bit clk = 0;

  initial repeat (4) #5 clk = ~clk;

  initial begin
    high = 8'h00;
    mid = 8'h00;
    low = 8'h00;

    {high, mid, low} <= 24'h123456;

    high_before = high;
    mid_before = mid;
    low_before = low;

    #1;
  end

  initial begin
    delayed_high = 8'h00;
    delayed_low = 8'h00;
    edged_high = 8'h00;
    edged_low = 8'h00;

    #1;
    {delayed_high, delayed_low} <= #3 16'hBEEF;
    {edged_high, edged_low} <= @(posedge clk) 16'hCAFE;

    // Time 3: the delay expires at 4 and the first posedge is at 5, so neither
    // share has landed.
    #2;
    delayed_high_midway = delayed_high;
    edged_high_midway = edged_high;
  end

  final begin
    if (high_before !== 8'h00)
      $fatal(1, "high_before was %h, expected 00", high_before);
    if (mid_before !== 8'h00)
      $fatal(1, "mid_before was %h, expected 00", mid_before);
    if (low_before !== 8'h00)
      $fatal(1, "low_before was %h, expected 00", low_before);

    if (high !== 8'h12) $fatal(1, "high was %h, expected 12", high);
    if (mid !== 8'h34) $fatal(1, "mid was %h, expected 34", mid);
    if (low !== 8'h56) $fatal(1, "low was %h, expected 56", low);

    if (delayed_high_midway !== 8'h00)
      $fatal(1, "the delayed share landed early, reading %h at time 3",
             delayed_high_midway);
    if (delayed_high !== 8'hBE)
      $fatal(1, "delayed_high was %h, expected BE", delayed_high);
    if (delayed_low !== 8'hEF)
      $fatal(1, "delayed_low was %h, expected EF", delayed_low);

    if (edged_high_midway !== 8'h00)
      $fatal(1, "the edge-controlled share landed early, reading %h at time 3",
             edged_high_midway);
    if (edged_high !== 8'hCA)
      $fatal(1, "edged_high was %h, expected CA", edged_high);
    if (edged_low !== 8'hFE)
      $fatal(1, "edged_low was %h, expected FE", edged_low);
    $display("All checks passed");
  end
endmodule
