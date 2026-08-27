// A fixed-size unpacked dimension is written either as a range [msb:lsb],
// whose bounds may be any integer values -- negative ones included -- and
// where msb may be greater than, equal to, or less than lsb, or as a single
// positive size N, which means the same as [0:N-1]. The declared range is
// what an index names: the indices inside it reach the elements and every
// index outside it is invalid, so a read through one yields the element
// type's default instead. A slice is written against the same range and in
// the same direction as the declaration (LRM 7.4.2, 7.4.5, Table 7-1).
module Top;
  logic [7:0] sized [3];
  logic [7:0] descending [5:3];
  logic [7:0] negative_base [-2:0];
  logic [7:0] single [4:4];

  integer index;

  logic [7:0] sized_at_0, sized_at_2;
  logic [7:0] descending_at_5, descending_at_3;
  logic [7:0] negative_at_minus_2, negative_at_0;
  logic [7:0] single_at_4;
  logic [7:0] descending_slice [2];

  logic [7:0] sized_at_3 = 8'h5A;
  logic [7:0] descending_at_2 = 8'h5A;
  logic [7:0] negative_at_1 = 8'h5A;
  logic [7:0] single_at_0 = 8'h5A;

  initial begin
    sized[0] = 8'h11;
    sized[1] = 8'h22;
    sized[2] = 8'h33;
    index = 0;
    sized_at_0 = sized[index];
    index = 2;
    sized_at_2 = sized[index];
    index = 3;
    sized_at_3 = sized[index];

    descending[5] = 8'hA5;
    descending[4] = 8'hB6;
    descending[3] = 8'hC7;
    index = 5;
    descending_at_5 = descending[index];
    index = 3;
    descending_at_3 = descending[index];
    index = 2;
    descending_at_2 = descending[index];
    descending_slice = descending[5:4];

    negative_base[-2] = 8'hD1;
    negative_base[-1] = 8'hD2;
    negative_base[0] = 8'hD3;
    index = -2;
    negative_at_minus_2 = negative_base[index];
    index = 0;
    negative_at_0 = negative_base[index];
    index = 1;
    negative_at_1 = negative_base[index];

    single[4] = 8'hE9;
    index = 4;
    single_at_4 = single[index];
    index = 0;
    single_at_0 = single[index];
  end

  final begin
    if (sized_at_0 !== 8'h11)
      $fatal(1, "sized_at_0 was %0h, expected 11", sized_at_0);
    if (sized_at_2 !== 8'h33)
      $fatal(1, "sized_at_2 was %0h, expected 33", sized_at_2);
    if (sized_at_3 !== 8'bxxxxxxxx)
      $fatal(1, "sized_at_3 was %0h, expected all x", sized_at_3);

    if (descending_at_5 !== 8'hA5)
      $fatal(1, "descending_at_5 was %0h, expected a5", descending_at_5);
    if (descending_at_3 !== 8'hC7)
      $fatal(1, "descending_at_3 was %0h, expected c7", descending_at_3);
    if (descending_at_2 !== 8'bxxxxxxxx)
      $fatal(1, "descending_at_2 was %0h, expected all x", descending_at_2);
    if (descending_slice[0] !== 8'hA5)
      $fatal(1, "descending_slice[0] was %0h, expected a5",
             descending_slice[0]);
    if (descending_slice[1] !== 8'hB6)
      $fatal(1, "descending_slice[1] was %0h, expected b6",
             descending_slice[1]);

    if (negative_at_minus_2 !== 8'hD1)
      $fatal(1, "negative_at_minus_2 was %0h, expected d1",
             negative_at_minus_2);
    if (negative_at_0 !== 8'hD3)
      $fatal(1, "negative_at_0 was %0h, expected d3", negative_at_0);
    if (negative_at_1 !== 8'bxxxxxxxx)
      $fatal(1, "negative_at_1 was %0h, expected all x", negative_at_1);

    if (single_at_4 !== 8'hE9)
      $fatal(1, "single_at_4 was %0h, expected e9", single_at_4);
    if (single_at_0 !== 8'bxxxxxxxx)
      $fatal(1, "single_at_0 was %0h, expected all x", single_at_0);
    $display("All checks passed");
  end
endmodule
