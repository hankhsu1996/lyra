// A queue declared with a right bound shall hold no element whose index is
// greater than that bound, so after any write that would leave it longer the
// elements past the bound are discarded and the ones within it stay in order.
// The bound belongs to the variable rather than to the value written, so it
// applies alike to a declaration initializer, a push, an unpacked array
// concatenation and a whole-queue assignment, and it still applies to every
// later write (LRM 7.10.5, 10.10).
module Top;
  bit [7:0] grown [$:3];
  bit [7:0] from_pattern [$:2] = '{1, 2, 3, 4, 5};
  bit [7:0] from_replication [$:2] = '{5{8'd7}};
  bit [7:0] from_concatenation [$:2];
  bit [7:0] partly_filled [$:3] = '{1, 2};
  bit [7:0] longer [$] = '{10, 20, 30, 40, 50};
  bit [7:0] from_queue [$:2];
  int cleared [$] = '{1, 2, 3};

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

    cleared = {};
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

    if (cleared.size() !== 0)
      $fatal(1, "cleared.size() was %0d, expected 0", cleared.size());
    $display("All checks passed");
  end
endmodule
