// @measure: run
// @work: queue pass
//
// One pass reads every element of a queue of 1024 through its index and
// writes each back changed, so what a pass costs is reaching an element of a
// queue where it lies, to read it and to write it (LRM 7.10).
module Top;
  localparam int QUEUE_SIZE = 1024;

  initial begin
    int num_passes;
    int checksum;
    int q[$];
    int element;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < QUEUE_SIZE; i++) q.push_back(i);

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < QUEUE_SIZE; i++) begin
        element = q[i];
        checksum = checksum + element;
        q[i] = element + pass;
      end
    end

    $display("queue-index done: checksum=%0d", checksum);
    $finish;
  end
endmodule
