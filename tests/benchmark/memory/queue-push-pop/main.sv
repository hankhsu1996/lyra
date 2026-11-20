// @measure: run
// @work: queue pass
//
// One pass fills a queue of 1024 elements by pushing each at its back, copies
// the whole queue, then empties the copy by popping each from its front, so
// what a pass costs is a queue's own operations: placing an element, building
// a copy of a whole value, taking an element out, and ending what is left.
module Top;
  localparam int QUEUE_SIZE = 1024;

  initial begin
    int num_passes;
    int checksum;
    int q[$];
    int copy[$];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < QUEUE_SIZE; i++) q.push_back(i + pass);
      copy = q;
      while (copy.size() > 0) checksum = checksum + copy.pop_front();
      q.delete();
    end

    $display("queue-push-pop done: checksum=%0d", checksum);
    $finish;
  end
endmodule
