// One fixed-size unpacked array may be assigned to another when the element
// types are equivalent and the two hold the same number of elements. The
// assignment copies element by element, and what corresponds to what is the
// left-to-right order of each array rather than the indices the elements
// carry: the leftmost element of the source lands in the leftmost element of
// the target, and the target keeps its own declared range afterwards. That
// holds at every dimension of a multidimensional array. The copy takes the
// values, so a later write to the source does not reach the target
// (LRM 7.6, 7.4.6).
module Top;
  int ascending [0:3] = '{11, 22, 33, 44};
  int descending [3:0] = '{1, 2, 3, 4};
  int independent [4] = '{5, 6, 7, 8};

  int reversed_ranges [3:1][4:1];
  int forward_ranges [1:3][1:4] = '{3{'{4{7}}}};

  initial begin
    descending = ascending;

    independent = ascending;
    ascending[0] = 999;

    reversed_ranges[3][4] = 91;
    reversed_ranges[2][3] = 93;
    reversed_ranges[1][1] = 92;
    forward_ranges = reversed_ranges;
  end

  final begin
    if (descending[3] !== 11)
      $fatal(1, "descending[3] was %0d, expected 11", descending[3]);
    if (descending[2] !== 22)
      $fatal(1, "descending[2] was %0d, expected 22", descending[2]);
    if (descending[0] !== 44)
      $fatal(1, "descending[0] was %0d, expected 44", descending[0]);

    if (independent[0] !== 11)
      $fatal(1, "independent[0] was %0d, expected 11", independent[0]);
    if (independent[3] !== 44)
      $fatal(1, "independent[3] was %0d, expected 44", independent[3]);
    if (ascending[0] !== 999)
      $fatal(1, "ascending[0] was %0d, expected 999", ascending[0]);

    if (forward_ranges[1][1] !== 91)
      $fatal(1, "forward_ranges[1][1] was %0d, expected 91",
             forward_ranges[1][1]);
    if (forward_ranges[2][2] !== 93)
      $fatal(1, "forward_ranges[2][2] was %0d, expected 93",
             forward_ranges[2][2]);
    if (forward_ranges[3][4] !== 92)
      $fatal(1, "forward_ranges[3][4] was %0d, expected 92",
             forward_ranges[3][4]);
    if (forward_ranges[1][2] !== 0)
      $fatal(1, "forward_ranges[1][2] was %0d, expected 0",
             forward_ranges[1][2]);
    $display("All checks passed");
  end
endmodule
