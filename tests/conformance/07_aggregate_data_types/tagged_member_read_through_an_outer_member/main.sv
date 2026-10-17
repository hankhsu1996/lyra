// A member of a tagged union may itself hold tagged unions -- an array of them,
// or a structure with one among its members -- and a member of one of those is
// read through the outer member with the usual dot notation, each tag on the
// way checked against the member the read names (LRM 7.3.2, 11.9) -- however
// large the inner member is beside the part read.
module Top;
  typedef union tagged {
    int a;
    int b;
  } choice_t;

  typedef choice_t choices_t[2];

  typedef struct {
    choice_t inner;
    int count;
  } holder_t;

  typedef union tagged {
    choices_t many;
    holder_t held;
  } outer_t;

  typedef union tagged {
    int big[4096];
    int single;
  } large_t;

  typedef union tagged {
    large_t inner;
    int other;
  } around_large_t;

  outer_t in_array;
  outer_t in_structure;
  around_large_t around_large;
  large_t large_value;
  choices_t built;
  int first_read;
  int second_read;
  int inner_read;
  int large_read;

  initial begin
    built[0] = tagged a 11;
    built[1] = tagged b 22;
    in_array = tagged many built;
    in_structure = tagged held '{inner: tagged b 33, count: 1};
    first_read = in_array.many[0].a;
    second_read = in_array.many[1].b;
    inner_read = in_structure.held.inner.b;
    large_value = tagged big '{default: 44};
    around_large = tagged inner large_value;
    large_read = around_large.inner.big[3];
  end

  final begin
    if (first_read !== 11)
      $fatal(1, "first_read was %0d, expected 11", first_read);
    if (second_read !== 22)
      $fatal(1, "second_read was %0d, expected 22", second_read);
    if (inner_read !== 33)
      $fatal(1, "inner_read was %0d, expected 33", inner_read);
    if (large_read !== 44)
      $fatal(1, "large_read was %0d, expected 44", large_read);
    $display("All checks passed");
  end
endmodule
