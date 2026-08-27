// An array assignment pattern is an array-valued source, so assigning one to
// a dynamic array sizes the array to the elements the pattern covers and then
// fills them, whatever size the array had before. A replication stands for a
// whole dimension, and a nested pattern gives each subarray its own element
// count, so the rows of one array may differ in length. An index-keyed
// pattern names the position each value lands at rather than taking the
// position from source order, and since every element of the array has to be
// covered by a key and no key may name an element the array does not have,
// the keys are what fix the size (LRM 10.9.1, 7.5, 7.6).
module Top;
  int positional [] = '{10, 20, 30};
  int replicated [] = '{4{7}};
  int jagged [][] = '{'{1, 2}, '{3, 4, 5}};
  int replicated_rows [][] = '{3{'{8, 9}}};

  int resized [];
  int keyed [];
  int keyed_out_of_order [];

  int resized_size;
  int resized0 = 77;
  int resized3 = 77;

  int keyed_size;
  int keyed0 = 77;
  int keyed2 = 77;

  int out_of_order_size;
  int out_of_order0 = 77;
  int out_of_order1 = 77;

  initial begin
    resized = new[2];
    resized[0] = 99;
    resized[1] = 99;
    resized = '{1, 2, 3, 4};
    resized_size = resized.size();
    resized0 = resized[0];
    resized3 = resized[3];

    keyed = '{0: 10, 1: 20, 2: 30};
    keyed_size = keyed.size();
    keyed0 = keyed[0];
    keyed2 = keyed[2];

    keyed_out_of_order = '{1: 66, 0: 55};
    out_of_order_size = keyed_out_of_order.size();
    out_of_order0 = keyed_out_of_order[0];
    out_of_order1 = keyed_out_of_order[1];
  end

  final begin
    if (positional.size() !== 3)
      $fatal(1, "positional.size() was %0d, expected 3", positional.size());
    if (positional[0] !== 10)
      $fatal(1, "positional[0] was %0d, expected 10", positional[0]);
    if (positional[2] !== 30)
      $fatal(1, "positional[2] was %0d, expected 30", positional[2]);

    if (replicated.size() !== 4)
      $fatal(1, "replicated.size() was %0d, expected 4", replicated.size());
    if (replicated[0] !== 7)
      $fatal(1, "replicated[0] was %0d, expected 7", replicated[0]);
    if (replicated[3] !== 7)
      $fatal(1, "replicated[3] was %0d, expected 7", replicated[3]);

    if (jagged.size() !== 2)
      $fatal(1, "jagged.size() was %0d, expected 2", jagged.size());
    if (jagged[0].size() !== 2)
      $fatal(1, "jagged[0].size() was %0d, expected 2", jagged[0].size());
    if (jagged[1].size() !== 3)
      $fatal(1, "jagged[1].size() was %0d, expected 3", jagged[1].size());
    if (jagged[1][2] !== 5)
      $fatal(1, "jagged[1][2] was %0d, expected 5", jagged[1][2]);

    if (replicated_rows.size() !== 3)
      $fatal(1, "replicated_rows.size() was %0d, expected 3",
             replicated_rows.size());
    if (replicated_rows[2].size() !== 2)
      $fatal(1, "replicated_rows[2].size() was %0d, expected 2",
             replicated_rows[2].size());
    if (replicated_rows[2][1] !== 9)
      $fatal(1, "replicated_rows[2][1] was %0d, expected 9",
             replicated_rows[2][1]);

    if (resized_size !== 4)
      $fatal(1, "resized_size was %0d, expected 4", resized_size);
    if (resized0 !== 1) $fatal(1, "resized0 was %0d, expected 1", resized0);
    if (resized3 !== 4) $fatal(1, "resized3 was %0d, expected 4", resized3);

    if (keyed_size !== 3)
      $fatal(1, "keyed_size was %0d, expected 3", keyed_size);
    if (keyed0 !== 10) $fatal(1, "keyed0 was %0d, expected 10", keyed0);
    if (keyed2 !== 30) $fatal(1, "keyed2 was %0d, expected 30", keyed2);

    if (out_of_order_size !== 2)
      $fatal(1, "out_of_order_size was %0d, expected 2", out_of_order_size);
    if (out_of_order0 !== 55)
      $fatal(1, "out_of_order0 was %0d, expected 55", out_of_order0);
    if (out_of_order1 !== 66)
      $fatal(1, "out_of_order1 was %0d, expected 66", out_of_order1);
    $display("All checks passed");
  end
endmodule
