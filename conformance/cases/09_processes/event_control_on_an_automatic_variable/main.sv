// An event control waits for a value change of an expression, whatever
// declared its operands (LRM 9.4.2), so a variable whose lifetime is a call or
// a block is waited on as a module's variable is. Only the process that
// declared it, the branches it forks and the subroutines it lends it to can
// reach it (LRM 6.21), and a write by any of them is a change the wait sees.
//
// A `ref` formal and its actual share one representation, so a change made
// through either is a change of the other (LRM 13.5.2). A wait on the formal
// therefore wakes on a write to the actual by name, whichever of the legal
// actuals it was handed -- a variable, an element of an unpacked array, a
// member of an unpacked structure, a class property -- and on a write through
// another reference to it. A `ref static` formal is waited on the same way.
//
// A formal of any other direction is a variable of the call too (LRM 13.5), so
// a branch the task forks writing it wakes a wait on it the same way. A
// function cannot wait (LRM 13.4.4), but the branches a `fork ... join_none`
// in one spawns can, on a variable of the function and on one of their own.
//
// Only a change of the waited expression is an event (LRM 9.4.2): a wait on an
// element does not wake when its sibling changes.
class Holder;
  int v;
endclass

module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;

  int whole;
  int arr[2];
  pair_t s;
  int for_static;
  Holder o = new;

  time task_local_woke_at = -1;
  time initial_local_woke_at = -1;
  time loop_var_woke_at = -1;
  time lent_local_woke_at = -1;
  int lent_local_after = -1;
  time formal_woke_at[string];
  int inout_actual;
  int output_actual;

  class Waiter;
    time woke_at = -1;
    task automatic run();
      int x;
      fork
        #4 x = 5;
      join_none
      @(x);
      woke_at = $time;
    endtask
  endclass

  task automatic task_local();
    int x;
    fork
      #1 x = 5;
    join_none
    @(x);
    task_local_woke_at = $time;
  endtask

  task automatic watch(ref int r, input string name);
    @(r);
    formal_woke_at[name] = $time;
  endtask

  task automatic watch_input(input int a);
    fork
      #7 a = 5;
    join_none
    @(a);
    formal_woke_at["input"] = $time;
  endtask

  task automatic watch_inout(inout int a);
    fork
      #8 a = 5;
    join_none
    @(a);
    formal_woke_at["inout"] = $time;
  endtask

  task automatic watch_output(output int a);
    fork
      #9 a = 5;
    join_none
    @(a);
    formal_woke_at["output"] = $time;
  endtask

  time function_local_woke_at = -1;
  time branch_local_woke_at = -1;

  function automatic void spawn_watchers();
    int x;
    fork
      begin
        @(x);
        function_local_woke_at = $time;
      end
      #30 x = 1;
      begin
        int y;
        fork
          #31 y = 1;
          begin
            @(y);
            branch_local_woke_at = $time;
          end
        join
      end
    join_none
  endfunction

  task automatic watch_static(ref static int r);
    @(r);
    formal_woke_at["static"] = $time;
  endtask

  // Writes the formal from one branch and waits on it from another, so the
  // only writer the wait can learn of is the reference itself.
  task automatic poke_and_watch(ref int r);
    fork
      #6 r = 5;
      begin
        @(r);
        lent_local_woke_at = $time;
      end
    join
  endtask

  task automatic lend_a_local();
    int x;
    poke_and_watch(x);
    lent_local_after = x;
  endtask

  initial task_local();

  initial begin
    automatic int z;
    fork
      #2 z = 5;
    join_none
    @(z);
    initial_local_woke_at = $time;
  end

  initial begin
    for (int i = 0; i < 1; i++) begin
      fork
        #3 i = 7;
      join_none
      @(i);
      loop_var_woke_at = $time;
    end
  end

  Waiter waiter = new;
  initial waiter.run();

  initial lend_a_local();
  initial watch_input(0);
  initial watch_inout(inout_actual);
  initial watch_output(output_actual);
  initial spawn_watchers();

  initial begin
    fork
      watch(whole, "whole");
      watch(arr[1], "element");
      watch(s.b, "member");
      watch(o.v, "property");
      watch_static(for_static);
    join_none
    #10 arr[0] = 1;
    #1 whole = 1;
    #1 arr[1] = 1;
    #1 s.b = 1;
    #1 o.v = 1;
    #1 for_static = 1;
  end

  final begin
    if (task_local_woke_at !== 1)
      $fatal(1, "a task's local woke at %0t, expected 1", task_local_woke_at);
    if (initial_local_woke_at !== 2)
      $fatal(1, "an automatic local of an initial woke at %0t, expected 2",
             initial_local_woke_at);
    if (loop_var_woke_at !== 3)
      $fatal(1, "a loop variable woke at %0t, expected 3", loop_var_woke_at);
    if (waiter.woke_at !== 4)
      $fatal(1, "a method's local woke at %0t, expected 4", waiter.woke_at);
    if (lent_local_woke_at !== 6)
      $fatal(1, "a lent local woke at %0t, expected 6", lent_local_woke_at);
    if (lent_local_after !== 5)
      $fatal(1, "the lent local was %0d after the call, expected 5",
             lent_local_after);
    if (formal_woke_at["input"] !== 7)
      $fatal(1, "an input formal woke at %0t, expected 7",
             formal_woke_at["input"]);
    if (formal_woke_at["inout"] !== 8)
      $fatal(1, "an inout formal woke at %0t, expected 8",
             formal_woke_at["inout"]);
    if (inout_actual !== 5)
      $fatal(1, "the inout actual was %0d, expected 5", inout_actual);
    if (formal_woke_at["output"] !== 9)
      $fatal(1, "an output formal woke at %0t, expected 9",
             formal_woke_at["output"]);
    if (output_actual !== 5)
      $fatal(1, "the output actual was %0d, expected 5", output_actual);
    if (formal_woke_at["whole"] !== 11)
      $fatal(1, "a formal lent a variable woke at %0t, expected 11",
             formal_woke_at["whole"]);
    if (formal_woke_at["element"] !== 12)
      $fatal(1, "a formal lent an element woke at %0t, expected 12",
             formal_woke_at["element"]);
    if (formal_woke_at["member"] !== 13)
      $fatal(1, "a formal lent a member woke at %0t, expected 13",
             formal_woke_at["member"]);
    if (formal_woke_at["property"] !== 14)
      $fatal(1, "a formal lent a property woke at %0t, expected 14",
             formal_woke_at["property"]);
    if (formal_woke_at["static"] !== 15)
      $fatal(1, "a ref static formal woke at %0t, expected 15",
             formal_woke_at["static"]);
    if (function_local_woke_at !== 30)
      $fatal(1, "a function's local woke a branch it spawned at %0t, expected 30",
             function_local_woke_at);
    if (branch_local_woke_at !== 31)
      $fatal(1, "a spawned branch's own local woke at %0t, expected 31",
             branch_local_woke_at);
    $display("All checks passed");
  end
endmodule
