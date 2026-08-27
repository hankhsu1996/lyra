// The num() method returns the number of members the enumeration has. That
// count is a property of the declaration alone: it follows neither the largest
// value in the enumeration nor the value the variable holds (LRM 6.19.5.5).
module Top;
  typedef enum {A = 10, B, C} few_t;
  typedef enum {P, Q, R, S, T} five_t;

  few_t f;
  five_t g;
  int few_count;
  int five_count;

  initial begin
    f = B;
    few_count = f.num();
    g = P;
    five_count = g.num();
  end

  final begin
    if (few_count !== 3) $fatal(1, "few_count was %0d, expected 3", few_count);
    if (five_count !== 5)
      $fatal(1, "five_count was %0d, expected 5", five_count);
    $display("All checks passed");
  end
endmodule
