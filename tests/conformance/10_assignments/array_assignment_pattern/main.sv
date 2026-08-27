// An array assignment pattern gives an array a value element for element,
// with the braces matching the array's dimensions and each expression
// evaluated in the context of an assignment to the element it lands in, so a
// narrow literal widens to the element type rather than warning about its
// size. A replication written inside a pattern stands for an entire single
// dimension. An index key names the element its value lands at instead of
// that element being taken from source order, so the keys may be written in
// any order. The default key covers the elements no index or type key
// matched, and every element of the array has to be covered by one of these
// rules -- including the elements of an array that already held a value
// (LRM 10.9, 10.9.1).
module Top;
  int positional [3] = '{10, 20, 30};
  int from_narrow_literals [2] = '{1'b1, 1'b0};
  logic [7:0] four_state_elements [3] = '{8'hAA, 8'hBB, 8'hCC};
  int nested [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};

  int replicated [4] = '{4{7}};
  int replicated_group [6] = '{2{1, 2, 3}};
  int replicated_nested [2][3] = '{2{'{1, 2, 3}}};

  int all_defaulted [4] = '{default: 99};
  int by_index [3] = '{0: 100, 1: 200, 2: 300};
  int by_index_out_of_order [3] = '{2: 300, 0: 100, 1: 200};
  int index_and_default [5] = '{0: 11, 2: 22, default: 33};
  int nested_default [2][3] = '{'{1, 2, 3}, '{default: 44}};

  int refilled [3] = '{1, 2, 3};

  initial refilled = '{default: 99};

  final begin
    if (positional[0] !== 10)
      $fatal(1, "positional[0] was %0d, expected 10", positional[0]);
    if (positional[2] !== 30)
      $fatal(1, "positional[2] was %0d, expected 30", positional[2]);

    if (from_narrow_literals[0] !== 1)
      $fatal(1, "from_narrow_literals[0] was %0d, expected 1",
             from_narrow_literals[0]);
    if (from_narrow_literals[1] !== 0)
      $fatal(1, "from_narrow_literals[1] was %0d, expected 0",
             from_narrow_literals[1]);

    if (four_state_elements[0] !== 8'hAA)
      $fatal(1, "four_state_elements[0] was %0h, expected aa",
             four_state_elements[0]);
    if (four_state_elements[2] !== 8'hCC)
      $fatal(1, "four_state_elements[2] was %0h, expected cc",
             four_state_elements[2]);

    if (nested[0][2] !== 3)
      $fatal(1, "nested[0][2] was %0d, expected 3", nested[0][2]);
    if (nested[1][0] !== 4)
      $fatal(1, "nested[1][0] was %0d, expected 4", nested[1][0]);

    if (replicated[0] !== 7)
      $fatal(1, "replicated[0] was %0d, expected 7", replicated[0]);
    if (replicated[3] !== 7)
      $fatal(1, "replicated[3] was %0d, expected 7", replicated[3]);

    if (replicated_group[0] !== 1)
      $fatal(1, "replicated_group[0] was %0d, expected 1",
             replicated_group[0]);
    if (replicated_group[2] !== 3)
      $fatal(1, "replicated_group[2] was %0d, expected 3",
             replicated_group[2]);
    if (replicated_group[3] !== 1)
      $fatal(1, "replicated_group[3] was %0d, expected 1",
             replicated_group[3]);
    if (replicated_group[5] !== 3)
      $fatal(1, "replicated_group[5] was %0d, expected 3",
             replicated_group[5]);

    if (replicated_nested[0][1] !== 2)
      $fatal(1, "replicated_nested[0][1] was %0d, expected 2",
             replicated_nested[0][1]);
    if (replicated_nested[1][0] !== 1)
      $fatal(1, "replicated_nested[1][0] was %0d, expected 1",
             replicated_nested[1][0]);
    if (replicated_nested[1][2] !== 3)
      $fatal(1, "replicated_nested[1][2] was %0d, expected 3",
             replicated_nested[1][2]);

    if (all_defaulted[0] !== 99)
      $fatal(1, "all_defaulted[0] was %0d, expected 99", all_defaulted[0]);
    if (all_defaulted[3] !== 99)
      $fatal(1, "all_defaulted[3] was %0d, expected 99", all_defaulted[3]);

    if (by_index[0] !== 100)
      $fatal(1, "by_index[0] was %0d, expected 100", by_index[0]);
    if (by_index[2] !== 300)
      $fatal(1, "by_index[2] was %0d, expected 300", by_index[2]);

    if (by_index_out_of_order[0] !== 100)
      $fatal(1, "by_index_out_of_order[0] was %0d, expected 100",
             by_index_out_of_order[0]);
    if (by_index_out_of_order[1] !== 200)
      $fatal(1, "by_index_out_of_order[1] was %0d, expected 200",
             by_index_out_of_order[1]);
    if (by_index_out_of_order[2] !== 300)
      $fatal(1, "by_index_out_of_order[2] was %0d, expected 300",
             by_index_out_of_order[2]);

    if (index_and_default[0] !== 11)
      $fatal(1, "index_and_default[0] was %0d, expected 11",
             index_and_default[0]);
    if (index_and_default[1] !== 33)
      $fatal(1, "index_and_default[1] was %0d, expected 33",
             index_and_default[1]);
    if (index_and_default[2] !== 22)
      $fatal(1, "index_and_default[2] was %0d, expected 22",
             index_and_default[2]);
    if (index_and_default[4] !== 33)
      $fatal(1, "index_and_default[4] was %0d, expected 33",
             index_and_default[4]);

    if (nested_default[0][1] !== 2)
      $fatal(1, "nested_default[0][1] was %0d, expected 2",
             nested_default[0][1]);
    if (nested_default[1][0] !== 44)
      $fatal(1, "nested_default[1][0] was %0d, expected 44",
             nested_default[1][0]);
    if (nested_default[1][2] !== 44)
      $fatal(1, "nested_default[1][2] was %0d, expected 44",
             nested_default[1][2]);

    if (refilled[0] !== 99)
      $fatal(1, "refilled[0] was %0d, expected 99", refilled[0]);
    if (refilled[2] !== 99)
      $fatal(1, "refilled[2] was %0d, expected 99", refilled[2]);
    $display("All checks passed");
  end
endmodule
