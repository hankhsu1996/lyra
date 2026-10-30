// A hierarchical name may be triggered off as well as read (LRM 23.6), so a
// variable reached through a path into a child instance is a source of the
// procedure that reads it like any other: it joins the implicit sensitivity
// list of an always_comb (LRM 9.2.2.2.1) and drives a continuous assignment
// (LRM 10.3.2). A change made after the first evaluation re-triggers both,
// however many instances the path descends through.
module Source;
  int value;
endmodule

module Mid;
  Source src();
endmodule

module Outer;
  Mid mid();
endmodule

module Top;
  Source a();
  Source b();
  Outer o();

  int summed;
  int assigned;
  int deep;

  always_comb summed = a.value + b.value;
  assign assigned = a.value + b.value;
  always_comb deep = o.mid.src.value;

  int summed_at_1;
  int assigned_at_1;
  int deep_at_1;

  initial begin
    a.value = 3;
    b.value = 11;
    o.mid.src.value = 5;
    #1;
    summed_at_1 = summed;
    assigned_at_1 = assigned;
    deep_at_1 = deep;
    a.value = 100;
    o.mid.src.value = 21;
    #1;
  end

  final begin
    if (summed_at_1 !== 14)
      $fatal(1, "summed_at_1 was %0d, expected 14", summed_at_1);
    if (assigned_at_1 !== 14)
      $fatal(1, "assigned_at_1 was %0d, expected 14", assigned_at_1);
    if (deep_at_1 !== 5)
      $fatal(1, "deep_at_1 was %0d, expected 5", deep_at_1);
    if (summed !== 111) $fatal(1, "summed was %0d, expected 111", summed);
    if (assigned !== 111) $fatal(1, "assigned was %0d, expected 111", assigned);
    if (deep !== 21) $fatal(1, "deep was %0d, expected 21", deep);
    $display("All checks passed");
  end
endmodule
