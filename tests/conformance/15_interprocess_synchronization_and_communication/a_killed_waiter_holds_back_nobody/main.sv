// A process waiting in a semaphore's get, a mailbox's get or a bounded
// mailbox's put waits for its turn among waiters in the order they arrived
// (LRM 15.3.3, 15.4.3, 15.4.5). One that stops waiting -- killed, as a
// timeout written with `disable fork` kills it (LRM 9.6.3), or left as a block
// around the call is disabled (LRM 9.6.2) -- waits no longer, so the waiters
// behind it are served as though it had never come, whether they arrived after
// it stopped or were already waiting behind it.
module Top;
  semaphore s;
  mailbox #(int) inbox;
  mailbox #(int) full;
  time sem_after_end = 0;
  time sem_behind = 0;
  time got_after_end = 0;
  int got_value = -1;
  time put_behind = 0;
  int kept[$];
  semaphore left_sem;
  mailbox #(int) left_box;
  time sem_after_leave = 0;
  int left_value = -1;
  time box_after_leave = 0;

  initial begin
    s = new(0);
    inbox = new;
    full = new(1);

    // A semaphore waiter killed before the next arrives.
    fork
      s.get(1);
      #1;
    join_any
    disable fork;
    fork
      begin
        s.get(1);
        sem_after_end = $time;
      end
      #2 s.put(1);
    join

    // A semaphore waiter killed while another waits behind it, with the keys
    // the one behind needs already there.
    fork
      begin
        fork
          s.get(5);
          #10;
        join_any
        disable fork;
      end
      begin
        #1 s.put(1);
        s.get(1);
        sem_behind = $time;
      end
    join

    // A mailbox get killed before a message arrives.
    fork
      begin
        int lost;
        inbox.get(lost);
      end
      #1;
    join_any
    disable fork;
    fork
      begin
        inbox.get(got_value);
        got_after_end = $time;
      end
      #1 inbox.put(42);
    join

    // A bounded put killed while another put waits behind it.
    full.put(1);
    fork
      begin
        fork
          full.put(2);
          #10;
        join_any
        disable fork;
      end
      begin
        #1 full.put(3);
        put_behind = $time;
      end
      begin
        int v;
        #20 full.get(v);
        kept.push_back(v);
        full.get(v);
        kept.push_back(v);
      end
    join
  end

  // A caller that leaves a disabled block around the call (LRM 9.6.2) stops
  // waiting as well, though its process goes on running.
  initial begin
    left_sem = new(0);
    left_box = new;
    fork
      begin
        begin : sem_wait
          left_sem.get(1);
        end
        #100;
      end
      #1 disable sem_wait;
      begin
        #2 left_sem.get(1);
        sem_after_leave = $time;
      end
      #3 left_sem.put(1);
      begin
        begin : box_wait
          int lost;
          left_box.get(lost);
        end
        #100;
      end
      #1 disable box_wait;
      begin
        #2 left_box.get(left_value);
        box_after_leave = $time;
      end
      #3 left_box.put(7);
    join
  end

  final begin
    if (sem_after_end !== 3)
      $fatal(1, "a semaphore get after a killed waiter ended at %0t, expected 3",
             sem_after_end);
    if (sem_behind !== 13)
      $fatal(1, "a semaphore get behind a killed waiter ended at %0t, expected 13",
             sem_behind);
    if (got_value !== 42 || got_after_end !== 15)
      $fatal(1, "a mailbox get after a killed waiter took %0d at %0t, expected 42 at 15",
             got_value, got_after_end);
    if (put_behind !== 35)
      $fatal(1, "a bounded put behind a killed waiter ended at %0t, expected 35",
             put_behind);
    if (kept.size() !== 2 || kept[0] !== 1 || kept[1] !== 3)
      $fatal(1, "a bounded mailbox kept a killed put's message or lost another");
    if (sem_after_leave !== 3)
      $fatal(1, "a semaphore get after a caller left a disabled block ended at %0t, expected 3",
             sem_after_leave);
    if (left_value !== 7 || box_after_leave !== 3)
      $fatal(1, "a mailbox get after a caller left a disabled block took %0d at %0t, expected 7 at 3",
             left_value, box_after_leave);
    $display("All checks passed");
  end
endmodule
