// The unsized dimension of an open array may be the packed one, and then the
// formal has no unpacked dimension at all: the actual supplies the width, and
// the queries at dimension 0 describe the packed part (LRM 35.5.6.1, Annex
// H.12.2). How many dimensions svDimensions counts for such a formal is not
// read here, since one clause numbers the packed part 0 and another gives the
// queries the meaning of $dimensions, which counts it. What they describe is
// not the range as written but the linearized, normalized one -- a packed
// array of range [L:R] is normalized to
// [abs(L-R):0], so an actual declared in ascending order reports a descending
// range of the same width (LRM Annex H.7.5, H.7.6). Such a formal may be an
// output or an inout too (LRM 35.5.6.1). Foreign code that writes neither
// leaves an inout actual as it was, while an output's value is undetermined
// (Annex H.6.3), so of an output only what the queries describe is asked.
module Top;
  import "DPI-C" function int packed_query(input logic [] v, input int which);
  import "DPI-C" function int packed_leave(output logic [] v);
  import "DPI-C" function int packed_keep(inout bit [] v);

  logic [11:0] narrow;
  logic [3:0] tiny;
  logic [0:7] ascending;

  int narrow_shape[7];
  int tiny_shape[7];
  int ascending_shape[7];
  logic [7:0] left;
  bit [3:0] kept;
  int left_size = -1;
  int kept_size = -1;

  initial begin
    narrow = 12'hABC;
    tiny = 4'h5;
    ascending = 8'h3C;
    for (int i = 0; i < 7; i++) begin
      narrow_shape[i] = packed_query(narrow, i);
      tiny_shape[i] = packed_query(tiny, i);
      ascending_shape[i] = packed_query(ascending, i);
    end
    left_size = packed_leave(left);
    kept = 4'b0011;
    kept_size = packed_keep(kept);
  end

  final begin
    if (narrow_shape[1] !== 11)
      $fatal(1, "the left bound of logic [11:0] was %0d, expected 11",
             narrow_shape[1]);
    if (narrow_shape[2] !== 0)
      $fatal(1, "the right bound of logic [11:0] was %0d, expected 0",
             narrow_shape[2]);
    if (narrow_shape[3] !== 0)
      $fatal(1, "the low bound of logic [11:0] was %0d, expected 0",
             narrow_shape[3]);
    if (narrow_shape[4] !== 11)
      $fatal(1, "the high bound of logic [11:0] was %0d, expected 11",
             narrow_shape[4]);
    if (narrow_shape[5] !== 12)
      $fatal(1, "the size of logic [11:0] was %0d, expected 12",
             narrow_shape[5]);
    if (narrow_shape[6] !== 1)
      $fatal(1, "the increment of logic [11:0] was %0d, expected 1",
             narrow_shape[6]);

    // The same formal, a narrower actual: the width came from the call.
    if (tiny_shape[1] !== 3)
      $fatal(1, "the left bound of logic [3:0] was %0d, expected 3",
             tiny_shape[1]);
    if (tiny_shape[5] !== 4)
      $fatal(1, "the size of logic [3:0] was %0d, expected 4", tiny_shape[5]);

    // An actual declared ascending is reported in its normalized form.
    if (ascending_shape[1] !== 7)
      $fatal(1, "the left bound of logic [0:7] was %0d, expected 7",
             ascending_shape[1]);
    if (ascending_shape[2] !== 0)
      $fatal(1, "the right bound of logic [0:7] was %0d, expected 0",
             ascending_shape[2]);
    if (ascending_shape[5] !== 8)
      $fatal(1, "the size of logic [0:7] was %0d, expected 8",
             ascending_shape[5]);
    if (ascending_shape[6] !== 1)
      $fatal(1, "the increment of logic [0:7] was %0d, expected 1",
             ascending_shape[6]);

    if (left_size !== 8)
      $fatal(1, "the size of an output logic [7:0] was %0d, expected 8",
             left_size);
    if (kept_size !== 4)
      $fatal(1, "the size of an inout bit [3:0] was %0d, expected 4",
             kept_size);
    if (kept !== 4'b0011)
      $fatal(1, "an inout bit [] the foreign code left came back as %b", kept);
    $display("All checks passed");
  end
endmodule
