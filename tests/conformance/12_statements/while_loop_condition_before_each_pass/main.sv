// A while-loop evaluates its control expression before each pass and repeats
// its statement for as long as that expression is true, so whatever the body
// changes is what ends the loop. A while-loop whose expression is not true when
// it is reached does not execute its statement at all (LRM 12.7.4).
module Top;
  int up;
  int sum;
  int down;
  int product;
  int value;
  int doublings;
  int guard;
  int hits;

  initial begin
    up = 0;
    sum = 0;
    while (up < 5) begin
      sum = sum + up;
      up = up + 1;
    end

    down = 4;
    product = 1;
    while (down > 0) begin
      product = product * down;
      down = down - 1;
    end

    value = 1;
    doublings = 0;
    while (value != 256) begin
      value = value * 2;
      doublings = doublings + 1;
    end

    guard = 5;
    hits = 0;
    while (guard < 0) begin
      hits = hits + 1;
      guard = guard + 1;
    end
  end

  final begin
    if (sum !== 10) $fatal(1, "sum was %0d, expected 10", sum);
    if (up !== 5) $fatal(1, "up was %0d, expected 5", up);
    if (product !== 24) $fatal(1, "product was %0d, expected 24", product);
    if (down !== 0) $fatal(1, "down was %0d, expected 0", down);
    if (value !== 256) $fatal(1, "value was %0d, expected 256", value);
    if (doublings !== 8) $fatal(1, "doublings was %0d, expected 8", doublings);
    if (hits !== 0) $fatal(1, "hits was %0d, expected 0", hits);
    if (guard !== 5) $fatal(1, "guard was %0d, expected 5", guard);
    $display("All checks passed");
  end
endmodule
