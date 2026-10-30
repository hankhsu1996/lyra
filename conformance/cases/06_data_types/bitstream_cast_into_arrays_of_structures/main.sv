// A bitstream cast fills its destination from the most significant bit of the
// source down: each element of an unpacked array in turn, and each member of a
// structure element in turn, at the width that element's own type fixes (LRM
// 6.24.3). Each structure declaration is a type of its own (LRM 6.22.1), so
// arrays of two structure types whose members are held alike -- a 4-state and a
// 2-state vector of different widths -- are each filled at their own widths,
// whichever is cast first.
module Top;
  typedef struct {
    logic [3:0] value;
  } narrow_t;

  typedef struct {
    bit [7:0] value;
  } wide_t;

  typedef narrow_t narrow_pair_t[2];
  typedef wide_t wide_pair_t[2];

  narrow_pair_t narrow;
  wide_pair_t wide;

  initial begin
    narrow[0].value = 4'h0;
    narrow[1].value = 4'h0;
    wide[0].value = 8'h00;
    wide[1].value = 8'h00;

    narrow = narrow_pair_t'(8'hA5);
    wide = wide_pair_t'(16'hA55A);
  end

  final begin
    if (narrow[0].value !== 4'hA)
      $fatal(1, "narrow[0] was %h, expected a", narrow[0].value);
    if (narrow[1].value !== 4'h5)
      $fatal(1, "narrow[1] was %h, expected 5", narrow[1].value);
    if (wide[0].value !== 8'hA5)
      $fatal(1, "wide[0] was %h, expected a5", wide[0].value);
    if (wide[1].value !== 8'h5A)
      $fatal(1, "wide[1] was %h, expected 5a", wide[1].value);
    $display("All checks passed");
  end
endmodule
