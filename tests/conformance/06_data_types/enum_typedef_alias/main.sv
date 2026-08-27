// A typedef gives an enumerated type a name so that the same type can be used
// wherever a declaration is allowed, a procedural block included. A further
// typedef of that name denotes the same type, not a distinct one, so a
// variable declared through it takes the same names without a cast and may be
// assigned from a variable declared through the original name
// (LRM 6.19.1, 6.19.3).
module Top;
  typedef enum {A = 3, B = 7, C = 9} primary_t;
  typedef primary_t alias_t;

  primary_t v1;
  alias_t v2;

  int alias_value;
  int cross_value;
  int local_value;
  string local_name;

  initial begin
    primary_t local_v;

    v1 = B;
    v2 = C;
    alias_value = v2;

    v2 = v1;
    cross_value = v2;

    local_v = C;
    local_value = local_v;
    local_name = local_v.name();
  end

  final begin
    if (alias_value !== 9)
      $fatal(1, "alias_value was %0d, expected 9", alias_value);
    if (cross_value !== 7)
      $fatal(1, "cross_value was %0d, expected 7", cross_value);
    if (local_value !== 9)
      $fatal(1, "local_value was %0d, expected 9", local_value);
    if (local_name !== "C")
      $fatal(1, "local_name was '%s', expected C", local_name);
    $display("All checks passed");
  end
endmodule
