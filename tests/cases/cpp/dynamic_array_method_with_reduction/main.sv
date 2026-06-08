module Top;
  int arr [];
  int sum_doubled;
  int sum_with_threshold;
  int product_plus_one;
  int threshold;

  initial begin
    arr = new[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 0;
    // LRM 7.12.3: sum with key = 2 * item; 2*10 + 2*20 + 2*0 = 60.
    sum_doubled = arr.sum with (item * 2);

    threshold = 5;
    // LRM 7.12.3: sum with conditional reads captured `threshold` via
    // ByReferenceCapture; both elements >= threshold contribute.
    sum_with_threshold = arr.sum with (item >= threshold ? item + threshold : 0);

    arr = new[3];
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    // LRM 7.12.3: product with key = item + 1; 2 * 3 * 4 = 24.
    product_plus_one = arr.product with (item + 1);
  end
endmodule
