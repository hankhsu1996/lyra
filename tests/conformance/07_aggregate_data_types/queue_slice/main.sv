// A queue slice Q[a:b] yields the b-a+1 elements lying between those indices. A
// bound below 0 counts as 0 and one above $ counts as $, so a slice that
// overhangs an end is trimmed to what the queue holds. It yields the queue with
// no elements when a is greater than b, when either bound carries x or z, and
// when a one-element slice names a position outside 0..$. The indexed forms
// Q[base+:width] and Q[base-:width] name the same pair of bounds and are
// trimmed the same way, and assigning Q[1:$] back to Q drops its first element
// (LRM 7.10.1, 7.10.4, 7.4.5).
module Top;
  int values [$] = '{10, 20, 30, 40, 50};
  logic [31:0] unknown_bound;

  int last_element;
  int element_before_last;

  int inner [$];
  int to_end [$];
  int without_last [$];
  int single [$];
  int reversed_bounds [$] = '{9, 9};
  int outside_range [$] = '{9, 9};
  int unknown_slice [$] = '{9, 9};
  int trimmed_low [$];
  int trimmed_high [$];
  int ascending_part [$];
  int descending_part [$];
  int ascending_trimmed [$];
  int descending_trimmed [$];
  int dropped [$] = '{1, 2, 3, 4};

  initial begin
    last_element = values[$];
    element_before_last = values[$-1];

    inner = values[1:3];
    to_end = values[1:$];
    without_last = values[0:$-1];
    single = values[2:2];
    reversed_bounds = values[3:1];
    outside_range = values[9:9];

    unknown_bound = 32'hx;
    unknown_slice = values[0:unknown_bound];

    trimmed_low = values[-2:1];
    trimmed_high = values[3:99];

    ascending_part = values[1+:3];
    descending_part = values[3-:2];
    ascending_trimmed = values[3+:5];
    descending_trimmed = values[1-:4];

    dropped = dropped[1:$];
    dropped = dropped[1:$];
  end

  final begin
    if (last_element !== 50)
      $fatal(1, "last_element was %0d, expected 50", last_element);
    if (element_before_last !== 40)
      $fatal(1, "element_before_last was %0d, expected 40",
             element_before_last);

    if (inner.size() !== 3)
      $fatal(1, "inner.size() was %0d, expected 3", inner.size());
    if (inner[0] !== 20) $fatal(1, "inner[0] was %0d, expected 20", inner[0]);
    if (inner[1] !== 30) $fatal(1, "inner[1] was %0d, expected 30", inner[1]);
    if (inner[2] !== 40) $fatal(1, "inner[2] was %0d, expected 40", inner[2]);

    if (to_end.size() !== 4)
      $fatal(1, "to_end.size() was %0d, expected 4", to_end.size());
    if (to_end[0] !== 20) $fatal(1, "to_end[0] was %0d, expected 20",
                                to_end[0]);
    if (to_end[3] !== 50) $fatal(1, "to_end[3] was %0d, expected 50",
                                to_end[3]);

    if (without_last.size() !== 4)
      $fatal(1, "without_last.size() was %0d, expected 4",
             without_last.size());
    if (without_last[0] !== 10)
      $fatal(1, "without_last[0] was %0d, expected 10", without_last[0]);
    if (without_last[3] !== 40)
      $fatal(1, "without_last[3] was %0d, expected 40", without_last[3]);

    if (single.size() !== 1)
      $fatal(1, "single.size() was %0d, expected 1", single.size());
    if (single[0] !== 30)
      $fatal(1, "single[0] was %0d, expected 30", single[0]);

    if (reversed_bounds.size() !== 0)
      $fatal(1, "reversed_bounds.size() was %0d, expected 0",
             reversed_bounds.size());
    if (outside_range.size() !== 0)
      $fatal(1, "outside_range.size() was %0d, expected 0",
             outside_range.size());
    if (unknown_slice.size() !== 0)
      $fatal(1, "unknown_slice.size() was %0d, expected 0",
             unknown_slice.size());

    if (trimmed_low.size() !== 2)
      $fatal(1, "trimmed_low.size() was %0d, expected 2", trimmed_low.size());
    if (trimmed_low[0] !== 10)
      $fatal(1, "trimmed_low[0] was %0d, expected 10", trimmed_low[0]);
    if (trimmed_low[1] !== 20)
      $fatal(1, "trimmed_low[1] was %0d, expected 20", trimmed_low[1]);

    if (trimmed_high.size() !== 2)
      $fatal(1, "trimmed_high.size() was %0d, expected 2", trimmed_high.size());
    if (trimmed_high[0] !== 40)
      $fatal(1, "trimmed_high[0] was %0d, expected 40", trimmed_high[0]);
    if (trimmed_high[1] !== 50)
      $fatal(1, "trimmed_high[1] was %0d, expected 50", trimmed_high[1]);

    if (ascending_part.size() !== 3)
      $fatal(1, "ascending_part.size() was %0d, expected 3",
             ascending_part.size());
    if (ascending_part[0] !== 20)
      $fatal(1, "ascending_part[0] was %0d, expected 20", ascending_part[0]);
    if (ascending_part[2] !== 40)
      $fatal(1, "ascending_part[2] was %0d, expected 40", ascending_part[2]);

    if (descending_part.size() !== 2)
      $fatal(1, "descending_part.size() was %0d, expected 2",
             descending_part.size());
    if (descending_part[0] !== 30)
      $fatal(1, "descending_part[0] was %0d, expected 30", descending_part[0]);
    if (descending_part[1] !== 40)
      $fatal(1, "descending_part[1] was %0d, expected 40", descending_part[1]);

    if (ascending_trimmed.size() !== 2)
      $fatal(1, "ascending_trimmed.size() was %0d, expected 2",
             ascending_trimmed.size());
    if (ascending_trimmed[0] !== 40)
      $fatal(1, "ascending_trimmed[0] was %0d, expected 40",
             ascending_trimmed[0]);
    if (ascending_trimmed[1] !== 50)
      $fatal(1, "ascending_trimmed[1] was %0d, expected 50",
             ascending_trimmed[1]);

    if (descending_trimmed.size() !== 2)
      $fatal(1, "descending_trimmed.size() was %0d, expected 2",
             descending_trimmed.size());
    if (descending_trimmed[0] !== 10)
      $fatal(1, "descending_trimmed[0] was %0d, expected 10",
             descending_trimmed[0]);
    if (descending_trimmed[1] !== 20)
      $fatal(1, "descending_trimmed[1] was %0d, expected 20",
             descending_trimmed[1]);

    if (dropped.size() !== 2)
      $fatal(1, "dropped.size() was %0d, expected 2", dropped.size());
    if (dropped[0] !== 3) $fatal(1, "dropped[0] was %0d, expected 3",
                                dropped[0]);
    if (dropped[1] !== 4) $fatal(1, "dropped[1] was %0d, expected 4",
                                dropped[1]);
    $display("All checks passed");
  end
endmodule
