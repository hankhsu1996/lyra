// Every class has randomize(), pre_randomize() and post_randomize() (LRM
// 18.6.1, 18.6.2) and a generator reached by srandom(), get_randstate() and
// set_randstate() (LRM 18.13.3 -- 18.13.5, 18.14.1), and semaphore, mailbox
// and weak_reference are classes (LRM 8.30, 15.1). So each of them has these
// methods through its handle, and a class extending one reaches the base's
// hooks through super, as 18.6.2 has a derived class's hook call its base's.
// None of them declares a random variable or a constraint, so randomize()
// succeeds, a state constraint decides it with nothing drawn (LRM 18.7), the
// mode methods act on nothing (LRM 18.8, 18.9), and the class's own methods
// still do what they did.
class A;
endclass

class Pool extends semaphore;
  int log = 0;

  function new();
    super.new(1);
  endfunction

  function void pre_randomize();
    super.pre_randomize();
    log = log * 10 + 1;
  endfunction

  function void post_randomize();
    super.post_randomize();
    log = log * 10 + 2;
  endfunction

  function void reseed(int seed);
    super.srandom(seed);
  endfunction
endclass

class Box extends mailbox #(int);
  int log = 0;

  function void post_randomize();
    super.post_randomize();
    log = log * 10 + 3;
  endfunction
endclass

class Ref extends weak_reference #(A);
  int log = 0;

  function new(A a);
    super.new(a);
  endfunction

  function void pre_randomize();
    super.pre_randomize();
    log = log * 10 + 4;
  endfunction
endclass

module Top;
  int lim = 3;
  int semaphore_randomized = -1;
  int semaphore_holds = -1;
  int semaphore_fails = -1;
  int mailbox_randomized = -1;
  int reference_randomized = -1;
  int pool_hooks = -1;
  int box_hooks = -1;
  int ref_hooks = -1;
  int keys_after = -1;
  int message = -1;
  string state_before = "unread";
  string state_after = "unset";

  initial begin
    semaphore s;
    mailbox #(int) m;
    A a;
    weak_reference #(A) w;
    Pool pool;
    Box box;
    Ref r;

    s = new(2);
    m = new;
    a = new;
    w = new(a);
    pool = new;
    box = new;
    r = new(a);

    semaphore_randomized = s.randomize();
    semaphore_holds = s.randomize() with { lim == 3; };
    semaphore_fails = s.randomize() with { lim == 4; };
    s.pre_randomize();
    s.post_randomize();
    s.rand_mode(0);
    s.constraint_mode(0);
    s.srandom(5);
    state_before = s.get_randstate();
    s.set_randstate(state_before);
    state_after = s.get_randstate();
    void'(s.try_get(2));
    keys_after = s.try_get(1);

    mailbox_randomized = m.randomize();
    m.srandom(6);
    m.post_randomize();
    m.put(9);
    m.get(message);

    reference_randomized = w.randomize();
    w.srandom(7);

    void'(pool.randomize());
    pool_hooks = pool.log;
    pool.reseed(8);
    void'(box.randomize());
    box_hooks = box.log;
    void'(r.randomize());
    ref_hooks = r.log;
  end

  final begin
    if (semaphore_randomized !== 1)
      $fatal(1, "a semaphore's randomize() answered %0d", semaphore_randomized);
    if (semaphore_holds !== 1)
      $fatal(1, "a semaphore's randomize() with a holding constraint answered %0d",
             semaphore_holds);
    if (semaphore_fails !== 0)
      $fatal(1, "a semaphore's randomize() with a failing constraint answered %0d",
             semaphore_fails);
    if (state_after != state_before)
      $fatal(1, "a semaphore's generator state did not survive set_randstate");
    if (keys_after !== 0)
      $fatal(1, "a semaphore left %0d keys after both were taken", keys_after);
    if (mailbox_randomized !== 1)
      $fatal(1, "a mailbox's randomize() answered %0d", mailbox_randomized);
    if (message !== 9)
      $fatal(1, "a mailbox gave back %0d, expected 9", message);
    if (reference_randomized !== 1)
      $fatal(1, "a weak reference's randomize() answered %0d",
             reference_randomized);
    if (pool_hooks !== 12)
      $fatal(1, "a semaphore's subclass ran hooks %0d, expected 12", pool_hooks);
    if (box_hooks !== 3)
      $fatal(1, "a mailbox's subclass ran hooks %0d, expected 3", box_hooks);
    if (ref_hooks !== 4)
      $fatal(1, "a weak reference's subclass ran hooks %0d, expected 4",
             ref_hooks);
    $display("All checks passed");
  end
endmodule
