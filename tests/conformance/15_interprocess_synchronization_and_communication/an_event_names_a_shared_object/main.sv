// An event variable is a handle to a synchronization object (LRM 6.17,
// 15.5.5): assigning one event to another makes both name one object, so a
// trigger through either reaches whoever waits on either; `null` names none,
// and triggering it does nothing; two events compare by the object they name;
// and a wait already under way stays on the object it began on, so an
// assignment reaches only later waits (LRM 15.5.5.1). A nonblocking trigger
// reaches the object its operand named where the statement stood, and one
// carrying an event control waits on the object that control's event named
// there. An event is
// a value wherever one may stand: a loop's condition, the initializer of a
// class property, and what a procedural continuous assignment or a force holds
// a variable to (LRM 10.6).
class Holder;
  event none = null;
endclass

module Top;
  event a, b, c;
  event alias_of_a = a;
  event none = null;
  int woken_through_b = 0;
  int woken_on_old = 0;
  int woken_on_new = 0;
  bit compared_distinct = 0;
  bit compared_same = 0;
  bit compared_case_same = 0;
  bit null_is_false = 0;
  bit null_equals_null = 0;
  bit fresh_is_true = 0;
  bit alias_triggered = 0;
  bit initializer_aliases = 0;
  bit nonblocking_frozen = 0;
  bit property_initialized_null = 0;
  int while_turns = 0;
  int for_turns = 0;
  int do_turns = 0;
  Holder holder = new;
  event taken, source_of_assign, source_of_force;
  int assigned_woken = 0;
  int forced_woken = 0;

  event e1, e2;
  event moving;
  event control_first, control_second, control_named, control_got;
  bit controlled_trigger_landed = 0;
  bit null_wait_woke = 0;
  bit null_triggered = 1;
  event held_first, held_second;

  initial begin
    // Before the merge, two declared events are two objects.
    compared_distinct = (a != b);
    a = b;
    compared_same = (a == b);
    compared_case_same = (a === b);
    null_is_false = !none;
    null_equals_null = (none == null);
    fresh_is_true = c ? 1'b1 : 1'b0;
    property_initialized_null = holder.none == null;
    -> none;
    while (c) begin
      while_turns++;
      if (while_turns == 3) break;
    end
    for (int i = 0; none; i++) for_turns++;
    do do_turns++; while (!c);

    fork
      begin
        @a;
        woken_through_b++;
      end
      begin
        #1;
        -> b;
      end
    join

    // `triggered` reads the object, whichever variable names it.
    -> b;
    alias_triggered = a.triggered;

    // `alias_of_a` was given `a`'s object at its declaration, before `a` was
    // made to name `b`'s.
    fork
      begin
        @alias_of_a;
        initializer_aliases = 1;
      end
      begin
        #1;
        -> alias_of_a;
      end
    join

    // LRM 15.5.5.1: the process waiting on `e2` waits on the object `e2` named
    // when it began; `e2 = e1` reaches only waits begun after it.
    fork
      begin
        @e2;
        woken_on_old++;
      end
      begin
        #1;
        e2 = e1;
        fork
          begin
            @e2;
            woken_on_new++;
          end
          begin
            #1;
            -> e2;
          end
        join
      end
    join_any
    #5;

    assign taken = source_of_assign;
    fork
      begin
        @taken;
        assigned_woken++;
      end
      begin
        #1;
        -> source_of_assign;
      end
    join
    deassign taken;
    force taken = source_of_force;
    fork
      begin
        @taken;
        forced_woken++;
      end
      begin
        #1;
        -> source_of_force;
      end
    join
    release taken;

    // A wait on an event naming nothing waits on nothing, which LRM 15.5.5.2
    // allows, and a trigger of it reaches nothing.
    fork
      begin
        @none;
        null_wait_woke = 1;
      end
      begin
        #1;
        -> none;
        null_triggered = none.triggered;
      end
    join_any
    #2;

    // The control is executed where the statement stands, on the object
    // `control_named` names then.
    control_named = control_first;
    ->> @(control_named) control_got;
    control_named = control_second;
    fork
      begin
        @control_got;
        controlled_trigger_landed = 1;
      end
      begin
        #1;
        -> control_first;
      end
    join_any
    #2;

    // The nonblocking trigger takes the object `moving` names now, and
    // `moving` is made to name another before the trigger lands.
    moving = held_first;
    fork
      begin
        @held_first;
        nonblocking_frozen = 1;
      end
      begin
        #1;
        ->> #2 moving;
        moving = held_second;
      end
    join_any
    #5;
  end

  final begin
    if (compared_distinct !== 1) $fatal(1, "two declared events compared equal");
    if (compared_same !== 1) $fatal(1, "an event assigned another did not compare equal to it");
    if (compared_case_same !== 1) $fatal(1, "case equality of merged events was not 1");
    if (null_is_false !== 1) $fatal(1, "an event naming no object did not test false");
    if (null_equals_null !== 1) $fatal(1, "an event given null did not compare equal to null");
    if (fresh_is_true !== 1) $fatal(1, "a declared event did not test true");
    if (property_initialized_null !== 1) $fatal(1, "a property initialized to null named an object");
    if (while_turns !== 3) $fatal(1, "a while on a declared event turned %0d times", while_turns);
    if (for_turns !== 0) $fatal(1, "a for on an event naming nothing turned %0d times", for_turns);
    if (do_turns !== 1) $fatal(1, "a do-while on a declared event turned %0d times", do_turns);
    if (woken_through_b !== 1) $fatal(1, "a trigger through b woke %0d waits on a", woken_through_b);
    if (alias_triggered !== 1) $fatal(1, "a.triggered did not see the trigger through b");
    if (initializer_aliases !== 1) $fatal(1, "an event initialized from another did not share its object");
    if (woken_on_old !== 0) $fatal(1, "a wait begun before the assignment was woken through the new object");
    if (woken_on_new !== 1) $fatal(1, "a wait begun after the assignment woke %0d times", woken_on_new);
    if (assigned_woken !== 1) $fatal(1, "an event assigned another by `assign` woke %0d times", assigned_woken);
    if (forced_woken !== 1) $fatal(1, "an event forced to another woke %0d times", forced_woken);
    if (null_wait_woke !== 0) $fatal(1, "a wait on an event naming nothing woke");
    if (null_triggered !== 0) $fatal(1, "an event naming nothing answered triggered");
    if (controlled_trigger_landed !== 1) $fatal(1, "a nonblocking trigger on an event control did not wait on the object named at the statement");
    if (nonblocking_frozen !== 1) $fatal(1, "a nonblocking trigger did not reach the object named at the statement");
    $display("All checks passed");
  end
endmodule
