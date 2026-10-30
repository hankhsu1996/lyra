// An array may be a formal argument of a subroutine, and the default
// mechanism copies it into the subroutine: an input formal is the caller's
// array copied whole, so an element the body writes and an element it adds
// are both invisible to the caller, while an output or inout formal is copied
// back whole when the call returns. A subroutine may also return an array as
// its result. This holds for every kind of array -- queue, dynamic,
// associative, and fixed-size unpacked -- and a fixed-size formal resolves a
// select against the range it was declared with, not one carried by the value
// (LRM 7.7, 13.5, 13.5.1).
module Top;
  typedef int qi_t[$];
  typedef int dai_t[];
  typedef int as_t[string];

  function automatic int sum_q(input qi_t q);
    int acc;
    acc = 0;
    foreach (q[i]) acc = acc + q[i];
    q[0] = 777;
    q.push_back(999);
    return acc;
  endfunction

  function automatic void scale_da(inout dai_t a, input int k);
    foreach (a[i]) a[i] = a[i] * k;
  endfunction

  function automatic void grow_da(inout dai_t a, input int v);
    dai_t old;
    old = a;
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
    m["scratch"] = -1;
    return m[key];
  endfunction

  function automatic void insert(inout as_t m, input string key, input int v);
    m[key] = v;
  endfunction

  function automatic int local_range_sum();
    int t[1:3];
    t[1] = 40;
    t[3] = 60;
    return t[1] + t[3];
  endfunction

  function automatic void fill_ua(output int a[2:4]);
    a[2] = 11;
    a[3] = 22;
    a[4] = 33;
  endfunction

  qi_t qv;
  dai_t dav;
  as_t amap;
  qi_t sq;
  int uav[2:4];

  int summed;
  int size_after_sum;
  int first_after_sum;
  int scaled_first;
  int scaled_second;
  int grown_size;
  int grown_last;
  int filled_size;
  int filled_first;
  int filled_second;
  int square_two;
  int square_three;
  int looked_up;
  int scratch_leaked;
  int inserted_existing;
  int inserted_new;
  int range_sum;
  int ua_low;
  int ua_high;

  initial begin
    qv.push_back(1);
    qv.push_back(2);
    qv.push_back(3);
    summed = sum_q(qv);
    size_after_sum = qv.size();
    first_after_sum = qv[0];

    dav = new[2];
    dav[0] = 5;
    dav[1] = 7;
    scale_da(dav, 3);
    scaled_first = dav[0];
    scaled_second = dav[1];

    grow_da(dav, 99);
    grown_size = dav.size();
    grown_last = dav[2];

    fill_q(qv);
    filled_size = qv.size();
    filled_first = qv[0];
    filled_second = qv[1];

    sq = squares(4);
    square_two = sq[2];
    square_three = sq[3];

    amap["alpha"] = 100;
    amap["beta"] = 200;
    looked_up = lookup(amap, "beta");
    scratch_leaked = amap.exists("scratch");

    insert(amap, "alpha", 111);
    insert(amap, "gamma", 300);
    inserted_existing = amap["alpha"];
    inserted_new = amap["gamma"];

    range_sum = local_range_sum();
    fill_ua(uav);
    ua_low = uav[2];
    ua_high = uav[4];
  end

  final begin
    if (summed !== 6) $fatal(1, "summed was %0d, expected 6", summed);
    if (size_after_sum !== 3)
      $fatal(1, "size_after_sum was %0d, expected 3", size_after_sum);
    if (first_after_sum !== 1)
      $fatal(1, "first_after_sum was %0d, expected 1", first_after_sum);
    if (scaled_first !== 15)
      $fatal(1, "scaled_first was %0d, expected 15", scaled_first);
    if (scaled_second !== 21)
      $fatal(1, "scaled_second was %0d, expected 21", scaled_second);
    if (grown_size !== 3)
      $fatal(1, "grown_size was %0d, expected 3", grown_size);
    if (grown_last !== 99)
      $fatal(1, "grown_last was %0d, expected 99", grown_last);
    if (filled_size !== 2)
      $fatal(1, "filled_size was %0d, expected 2", filled_size);
    if (filled_first !== 10)
      $fatal(1, "filled_first was %0d, expected 10", filled_first);
    if (filled_second !== 20)
      $fatal(1, "filled_second was %0d, expected 20", filled_second);
    if (square_two !== 4)
      $fatal(1, "square_two was %0d, expected 4", square_two);
    if (square_three !== 9)
      $fatal(1, "square_three was %0d, expected 9", square_three);
    if (looked_up !== 200)
      $fatal(1, "looked_up was %0d, expected 200", looked_up);
    if (scratch_leaked !== 0)
      $fatal(1, "scratch_leaked was %0d, expected 0", scratch_leaked);
    if (inserted_existing !== 111)
      $fatal(1, "inserted_existing was %0d, expected 111", inserted_existing);
    if (inserted_new !== 300)
      $fatal(1, "inserted_new was %0d, expected 300", inserted_new);
    if (range_sum !== 100)
      $fatal(1, "range_sum was %0d, expected 100", range_sum);
    if (ua_low !== 11) $fatal(1, "ua_low was %0d, expected 11", ua_low);
    if (ua_high !== 33) $fatal(1, "ua_high was %0d, expected 33", ua_high);
    $display("All checks passed");
  end
endmodule
