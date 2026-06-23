module Top;
  // LRM 7.12 array-manipulation methods on an associative receiver: reduction
  // (LRM 7.12.3), the locator family (LRM 7.12.1), and map (LRM 7.12.5). The
  // entry index is the key (LRM 7.12.1 / 7.12.4), so an index locator yields a
  // queue of the key type and `item.index` reads the key. Traversal follows the
  // array's key order (LRM 7.8). The ordering family (LRM 7.12.2) is excluded
  // for associative arrays and rejected by the frontend.
  int sa [string] = '{"a": 1, "b": 2, "c": 3};
  int ia [int] = '{10: 5, 20: 7, 30: 9};

  int r_sum, r_prod, r_xor, r_and, r_or;

  int v_find [$];
  string v_find_idx [$];
  int v_first [$];
  string v_first_idx [$];
  int v_last [$];
  string v_last_idx [$];
  int v_min [$];
  int v_max [$];
  int v_unique [$];

  int i_find_idx [$];
  int i_sum_key;

  int mapped [string];
  int m_a, m_b, m_c;

  initial begin
    r_sum = sa.sum;
    r_prod = sa.product;
    r_xor = sa.xor;
    r_and = sa.and;
    r_or = sa.or;

    v_find = sa.find with (item > 1);
    v_find_idx = sa.find_index with (item > 1);
    v_first = sa.find_first with (item > 1);
    v_first_idx = sa.find_first_index with (item > 1);
    v_last = sa.find_last with (item > 1);
    v_last_idx = sa.find_last_index with (item > 1);
    v_min = sa.min;
    v_max = sa.max;
    v_unique = sa.unique;

    // The index locator returns the integral keys, and `item.index` reads the
    // key inside the reduction.
    i_find_idx = ia.find_index with (item > 5);
    i_sum_key = ia.sum with (item.index);

    // map keeps the key set; the result is read back by key.
    mapped = sa.map with (item + 10);
    m_a = mapped["a"];
    m_b = mapped["b"];
    m_c = mapped["c"];
  end
endmodule
