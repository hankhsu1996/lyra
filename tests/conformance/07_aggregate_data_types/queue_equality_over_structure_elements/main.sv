// Two queues are compared element by element, and an element that is an
// unpacked structure is compared member by member as its own type declares
// them: a structure of 2-state members always yields a known bit, and one of
// 4-state members yields x where an x member leaves the relation ambiguous,
// which case equality instead takes as a value that has to match (LRM 11.2.2,
// 11.4.5, 7.10).
module Top;
  typedef struct {
    bit [7:0] value;
  } wide_t;

  typedef struct {
    logic [3:0] value;
  } narrow_t;

  wide_t wide [$];
  wide_t same_wide [$];
  wide_t other_wide [$];
  narrow_t narrow [$];
  narrow_t same_narrow [$];
  narrow_t other_narrow [$];

  logic wide_equal;
  logic wide_unequal;
  logic wide_case_match;
  logic narrow_ambiguous;
  logic narrow_settled;
  logic narrow_case_match;

  initial begin
    wide_equal = 1'b0;
    wide_unequal = 1'bx;
    wide_case_match = 1'b0;
    narrow_ambiguous = 1'b0;
    narrow_settled = 1'bx;
    narrow_case_match = 1'b0;

    wide.push_back('{value: 8'hA5});
    same_wide.push_back('{value: 8'hA5});
    other_wide.push_back('{value: 8'h5A});
    wide_equal = (wide == same_wide);
    wide_unequal = (wide == other_wide);
    wide_case_match = (wide === same_wide);

    narrow.push_back('{value: 4'b10x1});
    same_narrow.push_back('{value: 4'b10x1});
    other_narrow.push_back('{value: 4'b00x1});
    narrow_ambiguous = (narrow == same_narrow);
    narrow_settled = (narrow == other_narrow);
    narrow_case_match = (narrow === same_narrow);
  end

  final begin
    if (wide_equal !== 1'b1)
      $fatal(1, "wide_equal was %b, expected 1", wide_equal);
    if (wide_unequal !== 1'b0)
      $fatal(1, "wide_unequal was %b, expected 0", wide_unequal);
    if (wide_case_match !== 1'b1)
      $fatal(1, "wide_case_match was %b, expected 1", wide_case_match);
    if (narrow_ambiguous !== 1'bx)
      $fatal(1, "narrow_ambiguous was %b, expected x", narrow_ambiguous);
    if (narrow_settled !== 1'b0)
      $fatal(1, "narrow_settled was %b, expected 0", narrow_settled);
    if (narrow_case_match !== 1'b1)
      $fatal(1, "narrow_case_match was %b, expected 1", narrow_case_match);
    $display("All checks passed");
  end
endmodule
