// @reports: warning count=12
//
// An array assignment assigns each element of the source to the corresponding
// element of the target (LRM 7.6), and a queue's bound is no part of whether
// two types are equivalent (LRM 6.22.2), so an array of bounded queues takes an
// array whose queues have another bound. Each element is then a write to a
// bounded queue, which keeps only what its own bound admits and warns when it
// discards (LRM 7.10.5). That holds for every kind of array holding the queues,
// when the assignment also changes the outer kind, and at every depth, and an
// associative array's value for an absent index is carried as its entries are
// (LRM 7.9.9, 7.9.11). A bounded queue of bounded queues keeps what its own
// bound admits first, so a queue it discards is never written and warns of
// nothing. A source already within the target's bounds loses nothing, and a
// target of a wider bound keeps all it is given.
module Top;
  typedef int wide_q [$:3];
  typedef int narrow_q [$:1];

  wide_q fixed_wide [2];
  narrow_q fixed_narrow [2];
  wide_q dynamic_wide [];
  narrow_q dynamic_narrow [];
  wide_q keyed_wide [string];
  narrow_q keyed_narrow [string];
  wide_q queued_wide [$];
  narrow_q queued_narrow [$];

  narrow_q dynamic_from_fixed [];
  narrow_q fixed_from_dynamic [2];
  narrow_q fixed_from_queue [2];

  wide_q filler;
  wide_q filled_wide [int];
  narrow_q filled_narrow [int];

  wide_q bounded_wide [$:3];
  narrow_q bounded_narrow [$:1];

  wide_q nested_wide [2][2];
  narrow_q nested_narrow [2][2];

  narrow_q short_narrow [2];
  wide_q widened [2];
  wide_q within_bound [2];
  narrow_q within_target [2];

  initial begin
    repeat (4) bounded_wide.push_back('{27, 28, 29});
    bounded_narrow = bounded_wide;
  end

  initial begin
    fixed_wide = '{'{1, 2, 3}, '{4}};
    dynamic_wide = new[2];
    dynamic_wide[0] = '{5, 6, 7};
    dynamic_wide[1] = '{8};
    keyed_wide["k"] = '{9, 10, 11};
    queued_wide.push_back('{12, 13, 14});

    fixed_narrow = fixed_wide;
    dynamic_narrow = dynamic_wide;
    keyed_narrow = keyed_wide;
    queued_narrow = queued_wide;

    dynamic_from_fixed = fixed_wide;
    fixed_from_dynamic = dynamic_wide;
    fixed_from_queue = '{'{0}, '{0}};
    queued_wide.push_back('{15});
    fixed_from_queue = queued_wide;

    filler = '{24, 25, 26};
    filled_wide = '{default: filler};
    filled_narrow = filled_wide;

    nested_wide[1][0] = '{16, 17, 18};
    nested_narrow = nested_wide;

    short_narrow = '{'{19, 20}, '{21}};
    widened = short_narrow;
    within_bound = '{'{22, 23}, '{}};
    within_target = within_bound;
  end

  final begin
    if (fixed_narrow[0].size() !== 2 || fixed_narrow[0][1] !== 2 ||
        fixed_narrow[1].size() !== 1 || fixed_narrow[1][0] !== 4)
      $fatal(1, "a fixed-size array of bounded queues took %p", fixed_narrow);
    if (dynamic_narrow.size() !== 2 || dynamic_narrow[0].size() !== 2 ||
        dynamic_narrow[0][1] !== 6 || dynamic_narrow[1][0] !== 8)
      $fatal(1, "a dynamic array of bounded queues took %p", dynamic_narrow);
    if (keyed_narrow.num() !== 1 || keyed_narrow["k"].size() !== 2 ||
        keyed_narrow["k"][1] !== 10)
      $fatal(1, "an associative array of bounded queues took %p",
             keyed_narrow);
    if (queued_narrow.size() !== 1 || queued_narrow[0].size() !== 2 ||
        queued_narrow[0][1] !== 13)
      $fatal(1, "a queue of bounded queues took %p", queued_narrow);

    if (dynamic_from_fixed.size() !== 2 ||
        dynamic_from_fixed[0].size() !== 2 || dynamic_from_fixed[0][1] !== 2)
      $fatal(1, "a dynamic array from a fixed-size one took %p",
             dynamic_from_fixed);
    if (fixed_from_dynamic[0].size() !== 2 || fixed_from_dynamic[0][1] !== 6)
      $fatal(1, "a fixed-size array from a dynamic one took %p",
             fixed_from_dynamic);
    if (fixed_from_queue[0].size() !== 2 || fixed_from_queue[0][1] !== 13 ||
        fixed_from_queue[1].size() !== 1 || fixed_from_queue[1][0] !== 15)
      $fatal(1, "a fixed-size array from a queue took %p", fixed_from_queue);

    if (filled_narrow.num() !== 0 || filled_narrow[7].size() !== 2 ||
        filled_narrow[7][1] !== 25)
      $fatal(1, "an absent index of an array given a default read %p",
             filled_narrow[7]);

    if (bounded_narrow.size() !== 2 || bounded_narrow[1].size() !== 2 ||
        bounded_narrow[1][1] !== 28)
      $fatal(1, "a bounded queue of bounded queues took %p", bounded_narrow);

    if (nested_narrow[1][0].size() !== 2 || nested_narrow[1][0][1] !== 17 ||
        nested_narrow[0][0].size() !== 0)
      $fatal(1, "an array of arrays of bounded queues took %p",
             nested_narrow);

    if (widened[0].size() !== 2 || widened[0][1] !== 20 ||
        widened[1].size() !== 1)
      $fatal(1, "a wider bound took %p", widened);
    if (within_target[0].size() !== 2 || within_target[0][1] !== 23 ||
        within_target[1].size() !== 0)
      $fatal(1, "a source within the bound became %p", within_target);
    $display("All checks passed");
  end
endmodule
