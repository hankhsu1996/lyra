// The implicit sensitivity list of an always_comb holds the expansions of the
// longest static prefix of every net or variable identifier or select
// expression the block reads, excepting only what the block declares itself,
// what it also writes, and identifiers appearing solely in timing control
// (LRM 9.2.2.2.1). An element select whose index is a constant expression is
// itself a static prefix (LRM 11.5.3). None of that turns on the data type of
// what is read, so a string, a real, a member of an unpacked structure, and an
// element of a dynamic array each re-trigger the procedure when they change
// after time zero, exactly as an integral variable does. For the array element
// it makes no difference whether the write that changed it named that element
// or replaced the array as a whole, an array assignment assigning each element
// of the source to the corresponding element of the target (LRM 7.6).
module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;

  string text;
  int text_length;

  real level;
  int band;

  pair_t pair;
  int pair_sum;

  string source;
  string copy;

  int data[];
  int data_total;
  int total_after_whole_write = -1;
  int total_after_element_write = -1;

  always_comb text_length = text.len();
  always_comb band = (level > 2.0) ? 7 : 3;
  always_comb pair_sum = pair.a + pair.b;
  always_comb copy = source;
  always_comb data_total = data[0] + data[1];

  initial begin
    text = "ab";
    level = 0.5;
    pair.a = 1;
    pair.b = 2;
    source = "before";
    data = new [2];
    data[0] = 1;
    data[1] = 2;
    #1;
    text = "abcdef";
    level = 5.0;
    pair.b = 40;
    source = "after";
    data = '{30, 40};
    #1;
    total_after_whole_write = data_total;
    data[1] = 5;
    #1;
    total_after_element_write = data_total;
  end

  final begin
    if (text_length !== 6)
      $fatal(1, "text_length was %0d, expected 6", text_length);
    if (band !== 7) $fatal(1, "band was %0d, expected 7", band);
    if (pair_sum !== 41) $fatal(1, "pair_sum was %0d, expected 41", pair_sum);
    if (copy != "after") $fatal(1, "copy was %s, expected after", copy);
    if (total_after_whole_write !== 70)
      $fatal(1, "after the whole-array write the sum was %0d, expected 70",
             total_after_whole_write);
    if (total_after_element_write !== 35)
      $fatal(1, "after the single-element write the sum was %0d, expected 35",
             total_after_element_write);
    $display("All checks passed");
  end
endmodule
