module Top;
  int ia [] = '{5, 3, 8, 3, 1, 8};
  int empty_arr [];

  // find family (with clause mandatory, Boolean predicate).
  int q_find [$];
  int q_find_idx [$];
  int q_first [$];
  int q_first_idx [$];
  int q_last [$];
  int q_last_idx [$];
  int q_nomatch [$];

  // min / max / unique (with clause optional); index variants return queue<int>.
  int q_min [$];
  int q_max [$];
  int q_unique [$];
  int q_unique_idx [$];
  int q_min_empty [$];
  int q_minby [$];

  initial begin
    q_find          = ia.find             with (item > 3);
    q_find_idx      = ia.find_index       with (item > 3);
    q_first         = ia.find_first       with (item > 3);
    q_first_idx     = ia.find_first_index with (item > 3);
    q_last          = ia.find_last        with (item > 3);
    q_last_idx      = ia.find_last_index  with (item > 3);
    q_nomatch       = ia.find             with (item > 100);

    q_min           = ia.min;
    q_max           = ia.max;
    q_unique        = ia.unique;
    q_unique_idx    = ia.unique_index;
    q_min_empty     = empty_arr.min;
    q_minby         = ia.min with (10 - item);
  end
endmodule
