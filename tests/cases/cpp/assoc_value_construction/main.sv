module Top;
  // A declaration initializer runs the keyed literal through the structural
  // (constructor-time) path; the persistent default survives to the reads.
  int decl_tab [string] = '{"k": 5, default: 77};
  int dt_k, dt_miss;

  int tab [string];
  int t_a, t_b, t_miss, t_num;

  int cnt [int];
  int c_inc, c_miss, c_num;

  int src [string];
  int dst [string];
  int d_p, d_miss, d_num;

  initial begin
    dt_k = decl_tab["k"];
    dt_miss = decl_tab["zzz"];

    // LRM 7.9.11: a keyed literal with a persistent default. A read of a
    // nonexistent key returns the default and does not allocate, so num()
    // counts only the two written entries.
    tab = '{"a": 1, "b": 2, default: 99};
    t_a = tab["a"];
    t_b = tab["b"];
    t_miss = tab["xyz"];
    t_num = tab.num();

    // LRM 7.8.7: a compound write to a nonexistent entry seeds the entry from
    // the default before applying the operation.
    cnt = '{default: 1};
    cnt[5]++;
    c_inc = cnt[5];
    c_miss = cnt[9];
    c_num = cnt.num();

    // LRM 7.9.9: whole-array assignment clears the target then copies the
    // source entries; the default is part of the value and rides along.
    src = '{"p": 7, default: 88};
    dst = src;
    d_p = dst["p"];
    d_miss = dst["q"];
    d_num = dst.num();
  end
endmodule
