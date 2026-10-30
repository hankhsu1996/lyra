// An unpacked array concatenation builds a queue value from a comma-separated
// list: an operand that is itself an unpacked array contributes all of its
// elements in order, and an operand of the element type contributes one, with
// the results laid out left to right. The list with no items is the queue with
// no elements. Assigning such a concatenation back to the queue that appears
// inside it gives the push, pop and insert idioms (LRM 10.10, 7.10.4).
module Top;
  int pushed_back [$] = '{1, 2, 3};
  int pushed_front [$] = '{1, 2, 3};
  int inserted [$] = '{1, 2, 3, 4};
  int low [$] = '{10, 20};
  int high [$] = '{30, 40};
  int joined [$];
  int spliced [$];
  int emptied [$] = '{7, 8};
  int reseeded [$] = '{7, 8};

  initial begin
    pushed_back = {pushed_back, 4};
    pushed_front = {0, pushed_front};
    inserted = {inserted[0:1], 99, inserted[2:$]};
    joined = {low, high};
    spliced = {low, 25, high};
    emptied = {};
    reseeded = {};
    reseeded = {reseeded, 5};
  end

  final begin
    if (pushed_back.size() !== 4)
      $fatal(1, "pushed_back.size() was %0d, expected 4", pushed_back.size());
    if (pushed_back[0] !== 1)
      $fatal(1, "pushed_back[0] was %0d, expected 1", pushed_back[0]);
    if (pushed_back[3] !== 4)
      $fatal(1, "pushed_back[3] was %0d, expected 4", pushed_back[3]);

    if (pushed_front.size() !== 4)
      $fatal(1, "pushed_front.size() was %0d, expected 4", pushed_front.size());
    if (pushed_front[0] !== 0)
      $fatal(1, "pushed_front[0] was %0d, expected 0", pushed_front[0]);
    if (pushed_front[3] !== 3)
      $fatal(1, "pushed_front[3] was %0d, expected 3", pushed_front[3]);

    if (inserted.size() !== 5)
      $fatal(1, "inserted.size() was %0d, expected 5", inserted.size());
    if (inserted[0] !== 1)
      $fatal(1, "inserted[0] was %0d, expected 1", inserted[0]);
    if (inserted[1] !== 2)
      $fatal(1, "inserted[1] was %0d, expected 2", inserted[1]);
    if (inserted[2] !== 99)
      $fatal(1, "inserted[2] was %0d, expected 99", inserted[2]);
    if (inserted[3] !== 3)
      $fatal(1, "inserted[3] was %0d, expected 3", inserted[3]);
    if (inserted[4] !== 4)
      $fatal(1, "inserted[4] was %0d, expected 4", inserted[4]);

    if (joined.size() !== 4)
      $fatal(1, "joined.size() was %0d, expected 4", joined.size());
    if (joined[0] !== 10) $fatal(1, "joined[0] was %0d, expected 10",
                                joined[0]);
    if (joined[1] !== 20) $fatal(1, "joined[1] was %0d, expected 20",
                                joined[1]);
    if (joined[2] !== 30) $fatal(1, "joined[2] was %0d, expected 30",
                                joined[2]);
    if (joined[3] !== 40) $fatal(1, "joined[3] was %0d, expected 40",
                                joined[3]);

    if (spliced.size() !== 5)
      $fatal(1, "spliced.size() was %0d, expected 5", spliced.size());
    if (spliced[1] !== 20) $fatal(1, "spliced[1] was %0d, expected 20",
                                 spliced[1]);
    if (spliced[2] !== 25) $fatal(1, "spliced[2] was %0d, expected 25",
                                 spliced[2]);
    if (spliced[3] !== 30) $fatal(1, "spliced[3] was %0d, expected 30",
                                 spliced[3]);

    if (emptied.size() !== 0)
      $fatal(1, "emptied.size() was %0d, expected 0", emptied.size());

    if (reseeded.size() !== 1)
      $fatal(1, "reseeded.size() was %0d, expected 1", reseeded.size());
    if (reseeded[0] !== 5)
      $fatal(1, "reseeded[0] was %0d, expected 5", reseeded[0]);
    $display("All checks passed");
  end
endmodule
