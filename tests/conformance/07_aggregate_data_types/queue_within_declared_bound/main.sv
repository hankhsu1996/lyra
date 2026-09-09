// @reports-nothing:
//
// A bound is the greatest index a queue may hold, so a queue declared with one
// holds that many elements plus the one at index zero without ever exceeding
// it. A write that leaves the queue no longer than that discards nothing, and a
// tool has nothing to say about it -- the bound is a property the declaration
// states, not a limit reaching it is an error (LRM 7.10.5).
module Top;
  bit [7:0] pushed [$:2];
  bit [7:0] from_pattern [$:2] = '{1, 2, 3};
  bit [7:0] from_concatenation [$:2];
  bit [7:0] replicated [$:3] = '{4{8'd9}};

  initial begin
    pushed.push_back(8'd10);
    pushed.push_back(8'd20);
    pushed.push_back(8'd30);

    from_concatenation = {8'd7, 8'd8, 8'd9};
  end

  final begin
    if (pushed.size() !== 3)
      $fatal(1, "pushed held %0d elements, expected 3", pushed.size());
    if (pushed[2] !== 8'd30)
      $fatal(1, "pushed[2] was %0d, expected 30", pushed[2]);

    if (from_pattern.size() !== 3)
      $fatal(1, "from_pattern held %0d elements, expected 3",
             from_pattern.size());
    if (from_pattern[2] !== 8'd3)
      $fatal(1, "from_pattern[2] was %0d, expected 3", from_pattern[2]);

    if (from_concatenation.size() !== 3)
      $fatal(1, "from_concatenation held %0d elements, expected 3",
             from_concatenation.size());
    if (from_concatenation[0] !== 8'd7)
      $fatal(1, "from_concatenation[0] was %0d, expected 7",
             from_concatenation[0]);

    if (replicated.size() !== 4)
      $fatal(1, "replicated held %0d elements, expected 4", replicated.size());
    if (replicated[3] !== 8'd9)
      $fatal(1, "replicated[3] was %0d, expected 9", replicated[3]);
    $display("All checks passed");
  end
endmodule
