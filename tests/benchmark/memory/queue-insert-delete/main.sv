// @measure: run
// @work: queue pass
//
// One pass inserts an element into the middle of a queue of 1024 and deletes
// one at a position the passes move along, so what a pass costs is opening and
// closing a position inside a queue: every element after it changes position
// and none of them moves (LRM 7.10.2.2, 7.10.2.3, 7.10.3).
module Top;
  localparam int QUEUE_SIZE = 1024;

  initial begin
    int num_passes;
    int checksum;
    int q[$];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < QUEUE_SIZE; i++) q.push_back(i);

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      q.insert(QUEUE_SIZE / 2, pass);
      checksum = checksum + q[(pass + QUEUE_SIZE / 4) % QUEUE_SIZE];
      q.delete(pass % QUEUE_SIZE);
    end

    $display("queue-insert-delete done: checksum=%0d", checksum);
    $finish;
  end
endmodule
