// A variable of an enumerated type may be assigned one of that type's own
// names directly, but any other expression needs an explicit cast. The cast
// converts to the enumeration's base type without checking that the result is
// one of the names, so an out-of-range value can be put in the variable, and
// such a value has no name (LRM 6.19.3, 6.19.4, 6.19.5.6).
module Top;
  typedef enum {A = 1, B = 2, C = 4} val_t;

  val_t v;
  int direct;
  int in_range;
  int out_of_range;
  string out_of_range_name = "unset";

  initial begin
    v = C;
    direct = v;
    v = val_t'(2);
    in_range = v;
    v = val_t'(7);
    out_of_range = v;
    out_of_range_name = v.name();
  end

  final begin
    if (direct !== 4) $fatal(1, "direct was %0d, expected 4", direct);
    if (in_range !== 2) $fatal(1, "in_range was %0d, expected 2", in_range);
    if (out_of_range !== 7)
      $fatal(1, "out_of_range was %0d, expected 7", out_of_range);
    if (out_of_range_name !== "")
      $fatal(1, "out_of_range_name was '%s', expected the empty string",
             out_of_range_name);
    $display("All checks passed");
  end
endmodule
