// An open array formal whose only unsized dimension is the packed one has no
// unpacked dimension (LRM 35.5.6.1). The query functions number the packed
// part 0 and the unpacked dimensions from 1, and are given the meaning of the
// array queries (LRM Annex H.12.2), whose $dimensions counts the packed part
// as well (LRM 20.7). Which count svDimensions answers the standard does not
// settle. Lyra answers the number of unpacked dimensions, 0 here. This case
// holds Lyra to that.
module Top;
  import "DPI-C" function int dimensions_of(input logic [] v);

  logic [11:0] narrow;
  int counted;

  initial counted = dimensions_of(narrow);

  final begin
    if (counted !== 0)
      $fatal(1, "a 12-bit actual reported %0d dimensions, expected 0", counted);
    $display("All checks passed");
  end
endmodule
