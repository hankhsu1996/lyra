// An update event is a change in the state of a variable, and only a change is
// one (LRM 4.3). A procedure with an implicit sensitivity list is evaluated
// again for each update event on a variable it reads (LRM 9.2.2.2.1), so a
// write that leaves every variable it reaches as it was must not run the
// procedure again, whichever part of the variable the write named and however
// it was spelled -- a plain, compound or increment assignment to an element, a
// member reached through elements, a slice, the whole variable, a method
// changing an element in place, a subroutine writing through an element passed
// to it by reference (LRM 13.5.2).
//
// What a write reaches is not always what it names. Assigning to an
// associative array entry that does not exist allocates it (LRM 7.8.7), and
// assigning to Q[$+1] appends to the queue (LRM 7.10.1): each changes the
// variable even where the value written is the element type's default, which
// is what the new element would hold anyway. An index outside the array, on
// the other hand, makes the write ignored (LRM 7.4.6, 7.10.1), so it changes
// nothing whatever value it carried.
module Top;
  typedef struct {
    int tag;
    int vals[4];
  } record_t;

  int idx = 0;

  int mem[8];
  int mem_seen;
  int mem_runs;

  record_t recs[2];
  int recs_seen;
  int recs_runs;

  int lookup [string];
  int lookup_seen;
  int lookup_runs;

  int items [$];
  int items_seen;
  int items_runs;

  int lanes [2][$];
  int lanes_seen;
  int lanes_runs;

  always_comb begin
    mem_runs = mem_runs + 1;
    mem_seen = mem[idx];
  end

  always_comb begin
    recs_runs = recs_runs + 1;
    recs_seen = recs[idx].vals[idx];
  end

  always_comb begin
    lookup_runs = lookup_runs + 1;
    lookup_seen = lookup.num();
  end

  always_comb begin
    items_runs = items_runs + 1;
    items_seen = items.size();
  end

  always_comb begin
    lanes_runs = lanes_runs + 1;
    lanes_seen = lanes[idx].size();
  end

  function automatic void write_through(ref int target, input int value);
    target = value;
  endfunction

  // Each write runs in a time slot of its own, so the procedures it wakes have
  // run before the next one is made.
  task automatic expect_runs(input string what, input int got,
                             input int want);
    if (got !== want)
      $fatal(1, "%s: the procedure ran %0d times, expected %0d", what, got,
             want);
  endtask

  initial begin
    int base;
    #1;

    base = mem_runs;
    mem[2] = 5;
    #1 expect_runs("an element changed", mem_runs - base, 1);
    base = mem_runs;
    mem[2] = 5;
    #1 expect_runs("an element written with its own value", mem_runs - base,
                   0);
    base = mem_runs;
    mem[2] += 0;
    #1 expect_runs("an element added zero to", mem_runs - base, 0);
    base = mem_runs;
    mem[2]++;
    #1 expect_runs("an element incremented", mem_runs - base, 1);
    base = mem_runs;
    mem[idx + 9] = 7;
    #1 expect_runs("an element outside the array", mem_runs - base, 0);
    base = mem_runs;
    mem[4:5] = '{0, 0};
    #1 expect_runs("a slice written with its own values", mem_runs - base, 0);
    base = mem_runs;
    mem[4:5] = '{0, 3};
    #1 expect_runs("a slice with one element changed", mem_runs - base, 1);
    base = mem_runs;
    mem = mem;
    #1 expect_runs("the whole array written with itself", mem_runs - base, 0);
    base = mem_runs;
    write_through(mem[5], 3);
    #1 expect_runs("an element written by reference with its own value",
                   mem_runs - base, 0);
    base = mem_runs;
    write_through(mem[5], 3 + idx + 1);
    #1 expect_runs("an element changed by reference", mem_runs - base, 1);
    base = mem_runs;
    write_through(mem[5], 3);
    #1 expect_runs("an element changed back by reference", mem_runs - base, 1);
    if (mem[2] !== 6 || mem[5] !== 3)
      $fatal(1, "mem[2] was %0d and mem[5] %0d, expected 6 and 3", mem[2],
             mem[5]);

    base = recs_runs;
    recs[1].vals[2] = 0;
    #1 expect_runs("a nested member written with its own value",
                   recs_runs - base, 0);
    base = recs_runs;
    recs[1].vals[2] = 8;
    #1 expect_runs("a nested member changed", recs_runs - base, 1);

    base = lookup_runs;
    lookup["fresh"] = 0;
    #1 expect_runs("an entry allocated with the default value",
                   lookup_runs - base, 1);
    base = lookup_runs;
    lookup["fresh"] = 0;
    #1 expect_runs("an existing entry written with its own value",
                   lookup_runs - base, 0);
    if (lookup.num() !== 1)
      $fatal(1, "lookup held %0d entries, expected 1", lookup.num());

    base = items_runs;
    items[$+1] = 0;
    #1 expect_runs("an element appended at $+1 with the default value",
                   items_runs - base, 1);
    base = items_runs;
    items[5] = 4;
    #1 expect_runs("an element past $+1", items_runs - base, 0);
    if (items.size() !== 1)
      $fatal(1, "items held %0d elements, expected 1", items.size());

    base = lanes_runs;
    lanes[0].push_back(0);
    #1 expect_runs("an element changed in place by a method",
                   lanes_runs - base, 1);
    base = lanes_runs;
    lanes[0].sort();
    #1 expect_runs("an element a method left as it was", lanes_runs - base, 0);

    $display("All checks passed");
  end
endmodule
