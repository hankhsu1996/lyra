// @measure: run
// @work: queue pass
//
// The work of queue-push-pop over strings: one pass fills a queue of 1024
// elements by pushing each at its back, copies the whole queue, then empties
// the copy by popping each from its front. Each string is longer than a
// string keeps inside itself, so placing, copying and ending an element each
// reach storage of its own, which is what a queue of an element type with a
// copy and an end of its own costs.
module Top;
  localparam int QUEUE_SIZE = 1024;

  initial begin
    int num_passes;
    int checksum;
    string words[QUEUE_SIZE];
    string q[$];
    string copy[$];
    string word;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < QUEUE_SIZE; i++)
      words[i] = $sformatf("element number %0d of the queue", i);

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < QUEUE_SIZE; i++)
        q.push_back(words[(i + pass) % QUEUE_SIZE]);
      copy = q;
      while (copy.size() > 0) begin
        word = copy.pop_front();
        checksum = checksum + word.len() + word[15];
      end
      q.delete();
    end

    $display("queue-push-pop-string done: checksum=%0d", checksum);
    $finish;
  end
endmodule
