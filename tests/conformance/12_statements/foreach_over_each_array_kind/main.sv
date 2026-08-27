// A foreach-loop iterates over the elements of an array, giving its loop
// variable each index of the dimension in turn, and the element that index
// designates may be both read and written by the body. The array may be of any
// kind: a fixed-size unpacked array, a dynamic array, or a queue. A string is
// treated as a dynamic array of bytes indexed from 0 to N-1, where N is the
// number of characters. The array bounds supply the pass count the way a
// repeat-loop's expression does, so an array holding no elements runs the body
// no times (LRM 12.7.3).
module Top;
  int fixed_arr [5] = '{1, 2, 3, 4, 5};
  int fixed_sum;
  int fixed_order;

  int dyn [];
  int dyn_sum;
  int dyn_order;
  int dyn_sum_after_write;

  int empty_dyn [];
  int empty_passes;

  int q [$] = '{5, 6, 7};
  int q_sum;
  int q_order;
  int q_sum_after_write;

  string s;
  int s_sum;
  int s_order;
  byte s_last;

  initial begin
    fixed_sum = 0;
    fixed_order = 0;
    foreach (fixed_arr[i]) begin
      fixed_sum = fixed_sum + fixed_arr[i];
      fixed_order = fixed_order * 10 + i;
    end

    dyn = '{10, 20, 30};
    dyn_sum = 0;
    dyn_order = 0;
    foreach (dyn[i]) begin
      dyn_sum = dyn_sum + dyn[i];
      dyn_order = dyn_order * 10 + i;
    end
    foreach (dyn[i]) dyn[i] = i * 2;
    dyn_sum_after_write = 0;
    foreach (dyn[i]) dyn_sum_after_write = dyn_sum_after_write + dyn[i];

    empty_passes = 0;
    foreach (empty_dyn[i]) empty_passes = empty_passes + 1;

    q_sum = 0;
    q_order = 0;
    foreach (q[i]) begin
      q_sum = q_sum + q[i];
      q_order = q_order * 10 + i;
    end
    foreach (q[i]) q[i] = q[i] + 1;
    q_sum_after_write = 0;
    foreach (q[i]) q_sum_after_write = q_sum_after_write + q[i];

    s = "abc";
    s_sum = 0;
    s_order = 0;
    s_last = 0;
    foreach (s[i]) begin
      s_sum = s_sum + s[i];
      s_order = s_order * 10 + i;
      s_last = s[i];
      s[i] = s[i] + 1;
    end
  end

  final begin
    if (fixed_sum !== 15)
      $fatal(1, "fixed_sum was %0d, expected 15", fixed_sum);
    if (fixed_order !== 1234)
      $fatal(1, "fixed_order was %0d, expected 1234", fixed_order);
    if (dyn_sum !== 60) $fatal(1, "dyn_sum was %0d, expected 60", dyn_sum);
    if (dyn_order !== 12)
      $fatal(1, "dyn_order was %0d, expected 12", dyn_order);
    if (dyn_sum_after_write !== 6)
      $fatal(1, "dyn_sum_after_write was %0d, expected 6",
             dyn_sum_after_write);
    if (empty_passes !== 0)
      $fatal(1, "empty_passes was %0d, expected 0", empty_passes);
    if (q_sum !== 18) $fatal(1, "q_sum was %0d, expected 18", q_sum);
    if (q_order !== 12) $fatal(1, "q_order was %0d, expected 12", q_order);
    if (q_sum_after_write !== 21)
      $fatal(1, "q_sum_after_write was %0d, expected 21", q_sum_after_write);
    if (s_sum !== 294) $fatal(1, "s_sum was %0d, expected 294", s_sum);
    if (s_order !== 12) $fatal(1, "s_order was %0d, expected 12", s_order);
    if (s_last !== 99) $fatal(1, "s_last was %0d, expected 99", s_last);
    if (s !== "bcd") $fatal(1, "s was %s, expected bcd", s);
    $display("All checks passed");
  end
endmodule
