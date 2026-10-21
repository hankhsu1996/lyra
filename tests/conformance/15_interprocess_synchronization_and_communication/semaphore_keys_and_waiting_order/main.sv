// A semaphore is a bucket of keys (LRM 15.3): new() fills it, possibly below
// zero (15.3.1), put() returns keys and lets a waiter that now has enough go on
// (15.3.2), get() takes keys or waits for them, and its waiters are served in
// the order they arrived (15.3.3), so one asking for more at the head holds back
// a later one asking for fewer. try_get() takes keys only where get() would not
// have to wait (15.3.4). It is a class, so another class can extend it (15.2).
class Pool extends semaphore;
  int handed_out;
  function new(int keys);
    super.new(keys);
  endfunction
  task borrow();
    get(1);
    handed_out++;
  endtask
endclass

module Top;
  semaphore s;
  semaphore debt;
  Pool pool;
  time big_got = 0;
  time small_got = 0;
  int got_order[$];
  int try_first = -1;
  int try_queued = -1;
  int try_after = -1;
  int debt_try = -1;
  time debt_got = 0;
  time borrowed = 0;

  initial begin
    s = new;
    debt = new(-2);
    pool = new(0);
    try_first = s.try_get();
    fork
      begin
        s.get(2);
        big_got = $time;
        got_order.push_back(2);
      end
      begin
        #1 s.get(1);
        small_got = $time;
        got_order.push_back(1);
      end
      begin
        #2 s.put(1);
        try_queued = s.try_get(1);
        #1 s.put(2);
        #1 try_after = s.try_get(1);
      end
      begin
        debt_try = debt.try_get(1);
        debt.put(2);
        debt.put(1);
        debt.get(1);
        debt_got = $time;
      end
      begin
        pool.borrow();
        borrowed = $time;
      end
      #5 pool.put();
    join
  end

  final begin
    if (try_first !== 0)
      $fatal(1, "try_get on an empty bucket answered %0d, expected 0", try_first);
    if (try_queued !== 0)
      $fatal(1, "try_get past a waiter answered %0d, expected 0", try_queued);
    if (big_got !== 3)
      $fatal(1, "the first waiter got its keys at %0t, expected 3", big_got);
    if (small_got !== 3)
      $fatal(1, "the second waiter got its key at %0t, expected 3", small_got);
    if (got_order.size() !== 2 || got_order[0] !== 2 || got_order[1] !== 1)
      $fatal(1, "waiters were served out of their order of arrival");
    if (try_after !== 0)
      $fatal(1, "try_get on a bucket emptied by its waiters answered %0d, expected 0",
             try_after);
    if (debt_try !== 0)
      $fatal(1, "try_get below zero keys answered %0d, expected 0", debt_try);
    if (debt_got !== 0)
      $fatal(1, "a bucket started below zero gave its key at %0t, expected 0",
             debt_got);
    if (borrowed !== 5 || pool.handed_out !== 1)
      $fatal(1, "a class extending semaphore borrowed at %0t (%0d times), expected 5 (1)",
             borrowed, pool.handed_out);
    $display("All checks passed");
  end
endmodule
