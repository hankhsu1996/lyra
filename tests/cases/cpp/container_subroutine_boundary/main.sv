module Top;
  typedef int qi_t[$];
  typedef int dai_t[];
  typedef int as_t[string];

  function automatic int sum_q(input qi_t q);
    int acc = 0;
    foreach (q[i]) acc += q[i];
    return acc;
  endfunction

  function automatic void scale_da(inout dai_t a, input int k);
    foreach (a[i]) a[i] = a[i] * k;
  endfunction

  function automatic void grow_da(inout dai_t a, input int v);
    dai_t old = a;
    a = new[old.size() + 1];
    foreach (old[i]) a[i] = old[i];
    a[old.size()] = v;
  endfunction

  function automatic void fill_q(output qi_t q);
    q.push_back(10);
    q.push_back(20);
  endfunction

  function automatic qi_t squares(input int n);
    qi_t r;
    for (int i = 0; i < n; i++) r.push_back(i * i);
    return r;
  endfunction

  function automatic int lookup(input as_t m, input string key);
    return m[key];
  endfunction

  function automatic void bump(inout as_t m, input string key, input int v);
    m[key] = v;
  endfunction

  qi_t qv;
  dai_t dav;
  as_t amap;
  qi_t sq;

  int sum_rt;
  int da0_rt;
  int da1_rt;
  int grow_sz_rt;
  int grow_last_rt;
  int filled_sz_rt;
  int filled0_rt;
  int filled1_rt;
  int sq2_rt;
  int sq3_rt;
  int lookup_rt;
  int bump_existing_rt;
  int bump_new_rt;

  initial begin
    qv.push_back(1);
    qv.push_back(2);
    qv.push_back(3);
    sum_rt = sum_q(qv);

    dav = new[2];
    dav[0] = 5;
    dav[1] = 7;
    scale_da(dav, 3);
    da0_rt = dav[0];
    da1_rt = dav[1];

    grow_da(dav, 99);
    grow_sz_rt = dav.size();
    grow_last_rt = dav[2];

    fill_q(qv);
    filled_sz_rt = qv.size();
    filled0_rt = qv[0];
    filled1_rt = qv[1];

    sq = squares(4);
    sq2_rt = sq[2];
    sq3_rt = sq[3];

    amap["alpha"] = 100;
    amap["beta"] = 200;
    lookup_rt = lookup(amap, "beta");

    bump(amap, "alpha", 111);
    bump(amap, "gamma", 300);
    bump_existing_rt = amap["alpha"];
    bump_new_rt = amap["gamma"];
  end
endmodule
