module Top;
  // LRM 7.9 / 12.7.3: foreach over an associative array walks the entries in
  // index order (LRM 7.8: lexicographic for string, signed numeric for
  // integral). Each case inserts out of order and decodes the visit order into
  // a decimal number to prove the order, mirroring the traversal-method tests.

  int sm [string];
  int order_str;

  int im [int];
  int order_int;

  int em [string];
  int empty_iters;

  int mm [string][int];
  int sum_2d;

  int af [string][0:1];
  int sum_mixed;

  int bm [string][int];
  int break_count;

  int cm [int];
  int cont_sum;

  int fa [2][string];
  int sum_fixed_assoc;

  initial begin
    sm["banana"] = 2;
    sm["apple"] = 1;
    sm["cherry"] = 3;
    order_str = 0;
    foreach (sm[k]) order_str = order_str * 10 + sm[k];

    im[10] = 3;
    im[-5] = 2;
    im[5] = 1;
    order_int = 0;
    foreach (im[k]) order_int = order_int * 10 + im[k];

    empty_iters = 0;
    foreach (em[k]) empty_iters = empty_iters + 1;

    mm["x"][2] = 20;
    mm["x"][1] = 10;
    mm["y"][1] = 30;
    sum_2d = 0;
    foreach (mm[i, j]) sum_2d = sum_2d + mm[i][j];

    af["p"][0] = 1;
    af["p"][1] = 2;
    af["q"][0] = 4;
    sum_mixed = 0;
    foreach (af[i, j]) sum_mixed = sum_mixed + af[i][j];

    bm["a"][1] = 5;
    bm["a"][2] = 7;
    bm["b"][1] = 9;
    break_count = 0;
    foreach (bm[i, j]) begin
      if (bm[i][j] == 7) break;
      break_count = break_count + 1;
    end

    cm[1] = 1;
    cm[2] = 2;
    cm[3] = 3;
    cont_sum = 0;
    foreach (cm[k]) begin
      if (cm[k] == 2) continue;
      cont_sum = cont_sum + cm[k];
    end

    // A fixed outer dimension enclosing an associative inner one: the index walk
    // and the key walk nest in the same foreach.
    fa[0]["a"] = 1;
    fa[1]["a"] = 4;
    fa[1]["b"] = 2;
    sum_fixed_assoc = 0;
    foreach (fa[i, k]) sum_fixed_assoc = sum_fixed_assoc + fa[i][k];
  end
endmodule
