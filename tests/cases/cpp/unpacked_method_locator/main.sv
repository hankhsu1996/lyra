module Top;
  // LRM 7.12.1 locator methods on a fixed-size unpacked array receiver. Value
  // locators return a queue of the element type; index locators return a queue
  // of int. The find family takes a mandatory Boolean `with`; min / max /
  // unique take none.
  int a [6] = '{5, 3, 8, 3, 1, 8};

  int a_find [$];
  int a_find_idx [$];
  int a_first [$];
  int a_first_idx [$];
  int a_last [$];
  int a_last_idx [$];
  int a_nomatch [$];

  int a_min [$];
  int a_max [$];
  int a_unique [$];
  int a_unique_idx [$];

  initial begin
    a_find       = a.find             with (item > 3);
    a_find_idx   = a.find_index       with (item > 3);
    a_first      = a.find_first       with (item > 3);
    a_first_idx  = a.find_first_index with (item > 3);
    a_last       = a.find_last        with (item > 3);
    a_last_idx   = a.find_last_index  with (item > 3);
    a_nomatch    = a.find             with (item > 100);

    a_min        = a.min;
    a_max        = a.max;
    a_unique     = a.unique;
    a_unique_idx = a.unique_index;
  end
endmodule
