// try_get() takes keys where as many as it asks for are available (LRM
// 15.3.4), and the processes waiting in get() are served in the order they
// arrived (LRM 15.3.3). Where a get() is waiting at the head of the queue for
// more keys than the semaphore holds, and a try_get() asks for no more than it
// holds, the first clause has the try_get() succeed and the second can be read
// to keep the keys for the waiter; the standard does not say which governs.
// Lyra keeps them for the waiter, so the try_get() answers 0 and the waiter is
// served once enough keys are returned. This case holds Lyra to that.
module Top;
  semaphore s;
  int passed = -1;
  time waiter_got = 0;

  initial begin
    s = new;
    fork
      begin
        s.get(2);
        waiter_got = $time;
      end
      begin
        #1 s.put(1);
        passed = s.try_get(1);
        #1 s.put(1);
      end
    join
  end

  final begin
    if (passed !== 0)
      $fatal(1, "try_get past a waiter answered %0d, expected 0", passed);
    if (waiter_got !== 2)
      $fatal(1, "the waiter got its keys at %0t, expected 2", waiter_got);
    $display("All checks passed");
  end
endmodule
