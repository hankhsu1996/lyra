module Top;
  int q [$] = '{5, 6, 7};
  int sum_read;
  int sum_after_write;

  initial begin
    sum_read = 0;
    foreach (q[i]) sum_read = sum_read + q[i];

    foreach (q[i]) q[i] = q[i] + 1;
    sum_after_write = 0;
    foreach (q[i]) sum_after_write = sum_after_write + q[i];
  end
endmodule
