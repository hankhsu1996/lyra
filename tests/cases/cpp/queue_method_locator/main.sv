module Top;
  // LRM 7.12.1 locator methods on a queue receiver. Value locators return a
  // queue of the element type; index locators return a queue of int. The find
  // family takes a mandatory Boolean `with`; min / max / unique take none.
  int q [$] = '{5, 3, 8, 3, 1, 8};

  int q_find [$];
  int q_find_idx [$];
  int q_first [$];
  int q_first_idx [$];
  int q_last [$];
  int q_last_idx [$];
  int q_nomatch [$];

  int q_min [$];
  int q_max [$];
  int q_unique [$];
  int q_unique_idx [$];

  initial begin
    q_find       = q.find             with (item > 3);
    q_find_idx   = q.find_index       with (item > 3);
    q_first      = q.find_first       with (item > 3);
    q_first_idx  = q.find_first_index with (item > 3);
    q_last       = q.find_last        with (item > 3);
    q_last_idx   = q.find_last_index  with (item > 3);
    q_nomatch    = q.find             with (item > 100);

    q_min        = q.min;
    q_max        = q.max;
    q_unique     = q.unique;
    q_unique_idx = q.unique_index;
  end
endmodule
