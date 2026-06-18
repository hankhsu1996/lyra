module Top;
  // String-indexed: lexicographic order (LRM 7.8.2). first/last/next/prev plus
  // the empty and boundary cases.
  int sm [string];
  string sk;
  int s_first;
  int s_last;
  string s_last_key;
  int sum_fwd;
  int prod_bwd;
  int s_next_end;
  int s_prev_begin;

  int s_empty [string];
  string ek;
  int s_empty_status;

  // Integral-indexed: signed numerical order (LRM 7.8.4) and the LRM 7.9.8
  // narrow-argument case, where the key is truncated into the argument and the
  // method returns -1.
  int im [int];
  int ik;
  int sum_int_order;
  int last_int_key;

  int neg [int];
  int nk;
  int neg_first;

  initial begin
    sm["banana"] = 2;
    sm["apple"] = 1;
    sm["cherry"] = 3;

    s_first = sm.first(sk);
    s_last = sm.last(s_last_key);

    // Forward walk via first/next decodes the visit order into a decimal
    // number: apple(1), banana(2), cherry(3) -> 123.
    sum_fwd = 0;
    s_first = sm.first(sk);
    do sum_fwd = sum_fwd * 10 + sm[sk]; while (sm.next(sk));
    s_next_end = sm.next(sk);

    // Backward walk via last/prev: cherry(3), banana(2), apple(1) -> 321.
    prod_bwd = 0;
    s_last = sm.last(sk);
    do prod_bwd = prod_bwd * 10 + sm[sk]; while (sm.prev(sk));
    s_prev_begin = sm.prev(sk);

    s_empty_status = s_empty.first(ek);

    im[10] = 1;
    im[30] = 3;
    im[20] = 2;
    sum_int_order = 0;
    ik = 0;
    if (im.first(ik))
      do sum_int_order = sum_int_order * 10 + im[ik]; while (im.next(ik));
    last_int_key = ik;

    neg[5] = 1;
    neg[-5] = 2;
    neg_first = neg.first(nk);
  end
endmodule
