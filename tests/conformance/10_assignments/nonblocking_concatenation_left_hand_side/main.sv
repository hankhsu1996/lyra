// A nonblocking assignment whose left-hand side is a concatenation schedules
// every member of it and updates none of them until the end of the time step,
// so each member still reads its old value for the rest of the step and they
// all take their share of the distributed value together (LRM 10.4.2,
// Table 10-1).
module Top;
  logic [7:0] high, mid, low;
  logic [7:0] high_before, mid_before, low_before;

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
    $display("All checks passed");
  end
endmodule
