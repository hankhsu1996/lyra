module Top;
  int arr [];
  int weighted_sum;
  int first_two_sum;
  int weighted_sum_renamed;

  initial begin
    arr = new[4];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 0;
    // LRM 7.12.4 `item.index`: 0*10 + 1*20 + 2*30 + 3*0 = 80.
    weighted_sum = arr.sum with (item * item.index);
    // LRM 7.12.4 `item.index`: only the first two elements (index < 2)
    // contribute: 10 + 20 = 30.
    first_two_sum = arr.sum with (item.index < 2 ? item : 0);
    // LRM 7.12.4 two-name iterator/index argument form: rename the iterator to
    // `val` and the index method to `pos`. Same semantics as the default-name
    // form, so this matches `weighted_sum` at 80.
    weighted_sum_renamed = arr.sum(val, pos) with (val * val.pos);
  end
endmodule
