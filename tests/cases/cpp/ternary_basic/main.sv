module Top;
  int a;
  int b;
  int larger;
  int guarded_hit;
  int guarded_miss;
  initial begin
    a = 7;
    b = 3;
    larger = (a > b) ? a : b;

    // Conditional operator with an `&&&` multi-condition predicate (LRM 12.4):
    // the `?` branch is chosen iff every condition is true. `&&&` is a
    // cond_predicate separator, legal only at the predicate's top level (not
    // inside a parenthesized expression).
    guarded_hit  = (a > b) &&& (b > 0) ? a : b;
    guarded_miss = (a > b) &&& (b > 5) ? a : b;
  end
endmodule
