// A property is reached through a handle with the usual dot notation (LRM
// 8.4), and the handle is an operand of that access like any other: whatever
// side effects evaluating it has occur as the standard's rules give them (LRM
// 11.3.5), and an assignment operator evaluates its left-hand side once (LRM
// 11.4.1). So a function called to produce the handle runs once each time the
// access is evaluated -- for a read, a write, a write of an element, of a bit
// or of a structure member of the property, an assignment operator, a
// nonblocking assignment, and a built-in method changing the property.
//
// A property passed to a `ref` or an `inout` formal is an actual like any
// other (LRM 13.5): what reaches its handle is evaluated once, at the call. A
// call's result is not an lvalue, so the handle there comes from an element
// whose index a counted call computes.
//
// Writing a property changes its object, which reevaluates an event
// expression that reached the object (LRM 9.4.2).
module Top;
  typedef struct {
    int lo;
    int hi;
  } pair_t;

  class Holder;
    int         plain;
    int         many [0:3];
    logic [7:0] bits;
    int         count;
    pair_t      pair;
    int         queued [$];
    int         ticks;

    function void tick();
      ticks++;
    endfunction
  endclass

  Holder held;
  Holder pool [0:1];
  int    calls;

  function automatic Holder counted();
    calls = calls + 1;
    return held;
  endfunction

  function automatic int counted_index();
    calls = calls + 1;
    return 1;
  endfunction

  task automatic bump(ref int x);
    x += 10;
  endtask

  function automatic void twice(inout int x);
    x *= 2;
  endfunction

  int got;

  int on_read = -1;
  int on_write = -1;
  int on_element_write = -1;
  int on_bit_write = -1;
  int on_assignment_operator = -1;
  int on_nonblocking = -1;
  int on_loop_increment = -1;
  int on_loop_assignment = -1;
  int on_while_condition = -1;
  int on_do_condition = -1;
  int on_member_write = -1;
  int on_method = -1;
  int on_ref_actual = -1;
  int on_inout_actual = -1;
  int on_own_method = -1;
  int while_passes;
  int do_passes;
  time woke_at = 0;

  // Armed after time 0, when the object exists and its earlier writes are done.
  initial begin
    #1 @(held.plain);
    woke_at = $time;
  end

  initial begin
    held = new;
    pool[1] = new;
    held.bits = 8'h00;

    calls = 0;
    counted().plain = 4;
    on_write = calls;

    calls = 0;
    got = counted().plain;
    on_read = calls;

    calls = 0;
    counted().many[2] = 9;
    on_element_write = calls;

    calls = 0;
    counted().bits[3] = 1'b1;
    on_bit_write = calls;

    calls = 0;
    counted().plain += 1;
    on_assignment_operator = calls;

    calls = 0;
    counted().plain <= 11;
    on_nonblocking = calls;

    // "Once each time the access is evaluated" is once per evaluation: a loop's
    // step is evaluated after every iteration, so an increment written there
    // runs the handle's call once per step, and an assignment written there
    // does too. A while-loop's condition is evaluated before each pass and a
    // do...while-loop's after each (LRM 12.7.4, 12.7.5), so an increment
    // written in either runs the call once per evaluation of the condition,
    // the one that ends the loop included.
    calls = 0;
    for (int i = 0; i < 3; counted().many[0]++) i++;
    on_loop_increment = calls;

    calls = 0;
    for (int i = 0; i < 3; counted().many[1] = i) i++;
    on_loop_assignment = calls;

    held.count = 0;
    calls = 0;
    while (counted().count++ < 3) while_passes = while_passes + 1;
    on_while_condition = calls;

    held.count = 0;
    calls = 0;
    do do_passes = do_passes + 1; while (counted().count++ < 3);
    on_do_condition = calls;

    calls = 0;
    counted().pair.hi = 6;
    on_member_write = calls;

    calls = 0;
    counted().queued.push_back(8);
    on_method = calls;

    calls = 0;
    counted().tick();
    on_own_method = calls;

    pool[1].plain = 1;
    calls = 0;
    bump(pool[counted_index()].plain);
    on_ref_actual = calls;

    calls = 0;
    twice(pool[counted_index()].plain);
    on_inout_actual = calls;

    // The waiter above watches `held.plain`, which a write at time 5 changes.
    #5 counted().plain = 12;
  end

  final begin
    if (on_write !== 1)
      $fatal(1, "a write ran the handle's call %0d times, expected 1",
             on_write);
    if (on_read !== 1)
      $fatal(1, "a read ran the handle's call %0d times, expected 1", on_read);
    if (on_element_write !== 1)
      $fatal(1,
             "a write of an element ran the handle's call %0d times, expected 1",
             on_element_write);
    if (on_bit_write !== 1)
      $fatal(1, "a write of a bit ran the handle's call %0d times, expected 1",
             on_bit_write);
    if (on_assignment_operator !== 1)
      $fatal(1,
             "an assignment operator ran the handle's call %0d times, expected 1",
             on_assignment_operator);
    if (on_nonblocking !== 1)
      $fatal(1,
             "a nonblocking assignment ran the handle's call %0d times, expected 1",
             on_nonblocking);
    if (on_loop_increment !== 3)
      $fatal(1,
             "an increment in a loop's step ran the handle's call %0d times over 3 steps, expected 3",
             on_loop_increment);
    if (on_loop_assignment !== 3)
      $fatal(1,
             "an assignment in a loop's step ran the handle's call %0d times over 3 steps, expected 3",
             on_loop_assignment);
    if (held.many[0] !== 3)
      $fatal(1, "the loop left many[0] at %0d, expected 3", held.many[0]);
    if (while_passes !== 3)
      $fatal(1, "the while-loop made %0d passes, expected 3", while_passes);
    if (on_while_condition !== 4)
      $fatal(1,
             "a while-loop's condition ran the handle's call %0d times over 4 evaluations, expected 4",
             on_while_condition);
    if (do_passes !== 4)
      $fatal(1, "the do...while-loop made %0d passes, expected 4", do_passes);
    if (on_do_condition !== 4)
      $fatal(1,
             "a do...while-loop's condition ran the handle's call %0d times over 4 evaluations, expected 4",
             on_do_condition);
    if (held.count !== 4)
      $fatal(1, "the loops left count at %0d, expected 4", held.count);
    if (on_member_write !== 1)
      $fatal(1,
             "a write of a structure member ran the handle's call %0d times, expected 1",
             on_member_write);
    if (on_method !== 1)
      $fatal(1,
             "a method changing the property ran the handle's call %0d times, expected 1",
             on_method);
    if (on_own_method !== 1)
      $fatal(1, "a method call ran the handle's call %0d times, expected 1",
             on_own_method);
    if (on_ref_actual !== 1)
      $fatal(1,
             "a property passed by reference ran the index's call %0d times, expected 1",
             on_ref_actual);
    if (on_inout_actual !== 1)
      $fatal(1,
             "a property passed to an inout formal ran the index's call %0d times, expected 1",
             on_inout_actual);
    if (woke_at !== 5)
      $fatal(1, "a write to the property woke its waiter at %0t, expected 5",
             woke_at);
    // The accesses themselves took effect.
    if (got !== 4) $fatal(1, "the read answered %0d, expected 4", got);
    if (held.plain !== 12)
      $fatal(1, "the property ended as %0d, expected 12", held.plain);
    if (held.pair.hi !== 6)
      $fatal(1, "the structure member ended as %0d, expected 6", held.pair.hi);
    if (held.queued.size() !== 1 || held.queued[0] !== 8)
      $fatal(1, "the queue ended as %p, expected '{8}", held.queued);
    if (held.ticks !== 1)
      $fatal(1, "the method's own write left %0d, expected 1", held.ticks);
    if (pool[1].plain !== 22)
      $fatal(1, "the lent property ended as %0d, expected 22", pool[1].plain);
    if (held.many[2] !== 9)
      $fatal(1, "the element ended as %0d, expected 9", held.many[2]);
    if (held.bits !== 8'h08)
      $fatal(1, "the bits ended as %h, expected 08", held.bits);
    $display("All checks passed");
  end
endmodule
