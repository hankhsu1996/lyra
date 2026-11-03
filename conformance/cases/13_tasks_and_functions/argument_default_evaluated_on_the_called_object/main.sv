// A default of a method's formal is evaluated in the scope declaring the
// method each time a call omits the argument (LRM 13.5.3), which for a method
// is the object the call is made on (LRM 8.6). The source names that object
// once, so a call is made on one object whatever it leaves to defaults: the
// expression naming it is evaluated once, and the call stays on that object
// while the defaults are evaluated and the method runs, even where a default
// points the handle the source named at another object or lets go of every
// other handle to it (LRM 8.4). A default is evaluated only where the call
// using it is, so a call in an operand that does not run evaluates none, and
// a call standing as a loop's step evaluates it on each step (LRM 12.7.1).
typedef class Holder;

class Item;
  int n;
  Holder owner;
  static int last_dropped;
  static int sampled;

  function void two(int x, int y = 1, int z = 2);
    n = x + y + z;
  endfunction

  function void replaced(int x, int y = replace());
    n = x + y;
  endfunction

  function void dropped(int x, int y = drop());
    n = x + y;
    last_dropped = n;
  endfunction

  function void bump(int x = 1);
    n += x;
  endfunction

  function bit seen(int x, int y = 1);
    sampled = x + y;
    return 1;
  endfunction

  task automatic later(int x, int y = 5);
    #1 n = x + y;
  endtask

  function int replace();
    owner.item = new;
    return 1;
  endfunction

  function int drop();
    owner.item = null;
    owner = null;
    return 1;
  endfunction
endclass

class Holder;
  Item item;
  int picks;

  function Item pick();
    picks++;
    return item;
  endfunction

  function void through_property();
    item.two(0);
  endfunction

  function void through_property_one_left(int x);
    item.two(x, 4);
  endfunction

  function void through_local();
    Item local_item = new;
    local_item.two(10);
    item.n = local_item.n;
  endfunction

  function void through_call();
    pick().two(20);
  endfunction

  function void replaced_by_default();
    item.replaced(30);
  endfunction

  function void dropped_by_default();
    item.dropped(41);
  endfunction

  task automatic waited_through_property();
    item.later(50);
  endtask

  function void stepped();
    for (item.n = 0; item.n < 3; item.bump()) begin
    end
  endfunction

  function bit in_an_arm(bit take);
    return take && item.seen(60);
  endfunction
endclass

module Top;
  Holder h;
  Item old;
  Item kept;
  int results[10];

  initial begin
    h = new;
    h.item = new;

    h.through_property();
    results[0] = h.item.n;

    h.through_property_one_left(7);
    results[1] = h.item.n;

    h.through_local();
    results[2] = h.item.n;

    h.picks = 0;
    h.through_call();
    results[3] = h.picks * 100 + h.item.n;

    old = h.item;
    old.owner = h;
    h.replaced_by_default();
    results[4] = old.n * 100 + h.item.n;

    kept = h.item;
    kept.owner = h;
    h.item = kept;
    kept = null;
    h.dropped_by_default();
    results[5] = (h.item == null) * 100 + Item::last_dropped;

    h.item = new;
    old = h.item;
    h.waited_through_property();
    results[6] = old.n;

    // The second operand of `&&` runs only where the first is true (LRM
    // 11.4.7), and the call in it with it.
    h.item = new;
    Item::sampled = 0;
    results[7] = h.in_an_arm(0) * 100 + Item::sampled;
    results[8] = h.in_an_arm(1) * 100 + Item::sampled;

    h.stepped();
    results[9] = h.item.n;
  end

  final begin
    if (results[0] !== 3)
      $fatal(1, "a call through a property leaving two set %0d, expected 3",
             results[0]);
    if (results[1] !== 13)
      $fatal(1, "a call through a property leaving one set %0d, expected 13",
             results[1]);
    if (results[2] !== 13)
      $fatal(1, "a call through an automatic handle set %0d, expected 13",
             results[2]);
    if (results[3] !== 123)
      $fatal(1, "a call through a call gave %0d, expected 123 (once, 23)",
             results[3]);
    if (results[4] !== 3100)
      $fatal(1, "a default replacing the handle gave %0d, expected 3100",
             results[4]);
    if (results[5] !== 142)
      $fatal(1, "a default dropping the handle gave %0d, expected 142",
             results[5]);
    if (results[6] !== 55)
      $fatal(1, "a task through a property set %0d, expected 55", results[6]);
    if (results[7] !== 0)
      $fatal(1, "a call in an arm not taken gave %0d, expected 0",
             results[7]);
    if (results[8] !== 161)
      $fatal(1, "a call in an arm taken gave %0d, expected 161", results[8]);
    if (results[9] !== 3)
      $fatal(1, "a call as a loop's step reached %0d, expected 3", results[9]);
    $display("All checks passed");
  end
endmodule
