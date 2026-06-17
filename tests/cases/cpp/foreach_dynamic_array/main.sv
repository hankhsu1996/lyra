module Top;
  int a [];
  int e [];
  int sum_read;
  int sum_after_write;
  int empty_iters;

  initial begin
    a = '{10, 20, 30};

    sum_read = 0;
    foreach (a[i]) sum_read = sum_read + a[i];

    foreach (a[i]) a[i] = i * 2;
    sum_after_write = 0;
    foreach (a[i]) sum_after_write = sum_after_write + a[i];

    // A default-constructed dynamic array is empty (LRM Table 6-7), so the
    // foreach body never runs: the runtime size bound is just zero.
    empty_iters = 0;
    foreach (e[i]) empty_iters = empty_iters + 1;
  end
endmodule
