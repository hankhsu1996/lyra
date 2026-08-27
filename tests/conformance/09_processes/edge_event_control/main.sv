// A posedge is a transition toward 1 and a negedge a transition toward 0,
// which by Table 9-2 makes 0 to x, 0 to z and x or z to 1 posedges and 1 to x,
// 1 to z and x or z to 0 negedges; `edge` is either of them (LRM 9.4.2). On an
// expression wider than one bit the edge is detected only on the least
// significant bit of the expression, so a change confined to the other bits is
// no edge whatever it does to the value, and the same rule reads a bit-select,
// a part-select and an indexed part-select through the bits each of them names.
module Top;
  logic rising = 1'b0;
  logic falling = 1'b1;
  logic never_rises = 1'b1;
  logic both_ways = 1'b0;
  logic zero_to_x = 1'b0;
  logic x_to_one = 1'bx;
  logic one_to_x = 1'b1;
  logic x_to_zero = 1'bx;

  logic [7:0] whole = 8'h00;
  logic [7:0] noisy = 8'h00;
  logic [7:0] ranged = 8'h00;
  logic [7:0] ascending_part = 8'h00;
  logic [7:0] descending_part = 8'h00;

  time rising_at;
  time falling_at;
  int never_rises_woke;
  time both_first_at;
  time both_second_at;
  time zero_to_x_at;
  time x_to_one_at;
  time one_to_x_at;
  time x_to_zero_at;
  time whole_at;
  time noisy_at;
  time ranged_at;
  time ascending_part_at;
  time descending_part_at;

  initial begin
    @(posedge rising);
    rising_at = $time;
  end

  initial begin
    @(negedge falling);
    falling_at = $time;
  end

  initial begin
    @(posedge never_rises);
    never_rises_woke = 1;
  end

  initial begin
    @(edge both_ways);
    both_first_at = $time;
    @(edge both_ways);
    both_second_at = $time;
  end

  initial begin
    @(posedge zero_to_x);
    zero_to_x_at = $time;
  end

  initial begin
    @(posedge x_to_one);
    x_to_one_at = $time;
  end

  initial begin
    @(negedge one_to_x);
    one_to_x_at = $time;
  end

  initial begin
    @(negedge x_to_zero);
    x_to_zero_at = $time;
  end

  initial begin
    @(posedge whole);
    whole_at = $time;
  end

  initial begin
    @(posedge noisy[3]);
    noisy_at = $time;
  end

  initial begin
    @(posedge ranged[7:4]);
    ranged_at = $time;
  end

  initial begin
    @(posedge ascending_part[3 +: 4]);
    ascending_part_at = $time;
  end

  initial begin
    @(posedge descending_part[6 -: 4]);
    descending_part_at = $time;
  end

  initial begin
    #5;
    never_rises = 1'b0;
    whole = 8'hF0;
    noisy = 8'b0010_0000;
    ranged = 8'b1000_0000;
    ascending_part = 8'b0100_0000;
    descending_part = 8'b0100_0000;
    #5;
    rising = 1'b1;
    falling = 1'b0;
    both_ways = 1'b1;
    zero_to_x = 1'bx;
    x_to_one = 1'b1;
    one_to_x = 1'bx;
    x_to_zero = 1'b0;
    whole = 8'hF1;
    noisy[3] = 1'b1;
    ranged[4] = 1'b1;
    ascending_part[3] = 1'b1;
    descending_part[3] = 1'b1;
    #5;
    both_ways = 1'b0;
  end

  final begin
    if (rising_at !== 10)
      $fatal(1, "rising_at was %0d, expected 10", rising_at);
    if (falling_at !== 10)
      $fatal(1, "falling_at was %0d, expected 10", falling_at);
    if (never_rises_woke !== 0)
      $fatal(1, "never_rises_woke was %0d, expected 0", never_rises_woke);
    if (both_first_at !== 10)
      $fatal(1, "both_first_at was %0d, expected 10", both_first_at);
    if (both_second_at !== 15)
      $fatal(1, "both_second_at was %0d, expected 15", both_second_at);
    if (zero_to_x_at !== 10)
      $fatal(1, "zero_to_x_at was %0d, expected 10", zero_to_x_at);
    if (x_to_one_at !== 10)
      $fatal(1, "x_to_one_at was %0d, expected 10", x_to_one_at);
    if (one_to_x_at !== 10)
      $fatal(1, "one_to_x_at was %0d, expected 10", one_to_x_at);
    if (x_to_zero_at !== 10)
      $fatal(1, "x_to_zero_at was %0d, expected 10", x_to_zero_at);
    if (whole_at !== 10)
      $fatal(1, "whole_at was %0d, expected 10", whole_at);
    if (noisy_at !== 10)
      $fatal(1, "noisy_at was %0d, expected 10", noisy_at);
    if (ranged_at !== 10)
      $fatal(1, "ranged_at was %0d, expected 10", ranged_at);
    if (ascending_part_at !== 10)
      $fatal(1, "ascending_part_at was %0d, expected 10", ascending_part_at);
    if (descending_part_at !== 10)
      $fatal(1, "descending_part_at was %0d, expected 10",
             descending_part_at);
    $display("All checks passed");
  end
endmodule
