// @reports: warning count=9
//
// A queue declared with a right bound shall hold no element whose index is
// greater than that bound, so after any write that would leave it longer the
// elements past the bound are discarded and the ones within it stay in order.
// The bound belongs to the variable rather than to the value written, so it
// applies alike to a declaration initializer, a push, an unpacked array
// concatenation, a whole-queue assignment, an assignment from a fixed-size or a
// dynamic array, and the queue a locator method answers, and it still applies
// to every later write (LRM 7.6, 7.10.5, 7.12.1, 10.10). A slice of a bounded
// queue is a queue (LRM 7.10.1) and no variable, so it carries no bound into a
// queue of no bound it is joined into (LRM 7.10.5).
module Top;
  bit [7:0] grown [$:3];
  bit [7:0] from_pattern [$:2] = '{1, 2, 3, 4, 5};
  bit [7:0] from_replication [$:2] = '{5{8'd7}};
  bit [7:0] from_concatenation [$:2];
  bit [7:0] partly_filled [$:3] = '{1, 2};
  bit [7:0] longer [$] = '{10, 20, 30, 40, 50};
  bit [7:0] from_queue [$:2];
  bit [7:0] fixed_source [5] = '{11, 12, 13, 14, 15};
  bit [7:0] from_fixed [$:2];
  bit [7:0] dynamic_source [] = '{21, 22, 23, 24, 25};
  bit [7:0] from_dynamic [$:2];
  int pool [$] = '{5, 6, 7, 8, 9};
  int located [$:1];
  int cleared [$] = '{1, 2, 3};
  int pair [$:1] = '{1, 2};
  int joined_slice [$];

  initial begin
    grown.push_back(8'd10);
    grown.push_back(8'd20);
    grown.push_back(8'd30);
    grown.push_back(8'd40);
    grown.push_back(8'd50);

    from_concatenation = {8'd1, 8'd2, 8'd3, 8'd4};

    partly_filled.push_back(8'd3);
    partly_filled.push_back(8'd4);
    partly_filled.push_back(8'd5);

    from_queue = longer;
    from_fixed = fixed_source;
    from_dynamic = dynamic_source;
    located = pool.find with (item > 5);

    cleared = {};
    joined_slice = {pair[0:1], 3, 4};
  end

  final begin
    if (grown.size() !== 4)
      $fatal(1, "grown.size() was %0d, expected 4", grown.size());
    if (grown[0] !== 8'd10) $fatal(1, "grown[0] was %0d, expected 10",
                                  grown[0]);
    if (grown[1] !== 8'd20) $fatal(1, "grown[1] was %0d, expected 20",
                                  grown[1]);
    if (grown[2] !== 8'd30) $fatal(1, "grown[2] was %0d, expected 30",
                                  grown[2]);
    if (grown[3] !== 8'd40) $fatal(1, "grown[3] was %0d, expected 40",
                                  grown[3]);

    if (from_pattern.size() !== 3)
      $fatal(1, "from_pattern.size() was %0d, expected 3",
             from_pattern.size());
    if (from_pattern[0] !== 8'd1)
      $fatal(1, "from_pattern[0] was %0d, expected 1", from_pattern[0]);
    if (from_pattern[1] !== 8'd2)
      $fatal(1, "from_pattern[1] was %0d, expected 2", from_pattern[1]);
    if (from_pattern[2] !== 8'd3)
      $fatal(1, "from_pattern[2] was %0d, expected 3", from_pattern[2]);

    if (from_replication.size() !== 3)
      $fatal(1, "from_replication.size() was %0d, expected 3",
             from_replication.size());
    if (from_replication[2] !== 8'd7)
      $fatal(1, "from_replication[2] was %0d, expected 7",
             from_replication[2]);

    if (from_concatenation.size() !== 3)
      $fatal(1, "from_concatenation.size() was %0d, expected 3",
             from_concatenation.size());
    if (from_concatenation[0] !== 8'd1)
      $fatal(1, "from_concatenation[0] was %0d, expected 1",
             from_concatenation[0]);
    if (from_concatenation[2] !== 8'd3)
      $fatal(1, "from_concatenation[2] was %0d, expected 3",
             from_concatenation[2]);

    if (partly_filled.size() !== 4)
      $fatal(1, "partly_filled.size() was %0d, expected 4",
             partly_filled.size());
    if (partly_filled[0] !== 8'd1)
      $fatal(1, "partly_filled[0] was %0d, expected 1", partly_filled[0]);
    if (partly_filled[3] !== 8'd4)
      $fatal(1, "partly_filled[3] was %0d, expected 4", partly_filled[3]);

    if (longer.size() !== 5)
      $fatal(1, "longer.size() was %0d, expected 5", longer.size());
    if (from_queue.size() !== 3)
      $fatal(1, "from_queue.size() was %0d, expected 3", from_queue.size());
    if (from_queue[0] !== 8'd10)
      $fatal(1, "from_queue[0] was %0d, expected 10", from_queue[0]);
    if (from_queue[1] !== 8'd20)
      $fatal(1, "from_queue[1] was %0d, expected 20", from_queue[1]);
    if (from_queue[2] !== 8'd30)
      $fatal(1, "from_queue[2] was %0d, expected 30", from_queue[2]);

    if (from_fixed.size() !== 3)
      $fatal(1, "from_fixed.size() was %0d, expected 3", from_fixed.size());
    if (from_fixed[0] !== 8'd11 || from_fixed[2] !== 8'd13)
      $fatal(1, "from_fixed held %0d .. %0d, expected 11 .. 13", from_fixed[0],
             from_fixed[2]);

    if (from_dynamic.size() !== 3)
      $fatal(1, "from_dynamic.size() was %0d, expected 3",
             from_dynamic.size());
    if (from_dynamic[0] !== 8'd21 || from_dynamic[2] !== 8'd23)
      $fatal(1, "from_dynamic held %0d .. %0d, expected 21 .. 23",
             from_dynamic[0], from_dynamic[2]);

    if (located.size() !== 2)
      $fatal(1, "located.size() was %0d, expected 2", located.size());
    if (located[0] !== 6 || located[1] !== 7)
      $fatal(1, "located held %0d %0d, expected 6 7", located[0], located[1]);

    if (cleared.size() !== 0)
      $fatal(1, "cleared.size() was %0d, expected 0", cleared.size());

    if (joined_slice.size() !== 4)
      $fatal(1, "joined_slice.size() was %0d, expected 4", joined_slice.size());
    if (joined_slice[1] !== 2 || joined_slice[3] !== 4)
      $fatal(1, "joined_slice held %0d .. %0d, expected 2 .. 4",
             joined_slice[1], joined_slice[3]);
    $display("All checks passed");
  end
endmodule
