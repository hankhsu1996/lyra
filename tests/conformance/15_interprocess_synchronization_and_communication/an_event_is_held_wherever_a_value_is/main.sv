// An event is a data type (LRM 6.17), so it is held wherever a value is: an
// element of every kind of array (LRM 7.4, 7.5, 7.8, 7.10), a member of a
// structure, a property of a class, a variable of a package, an argument and a
// result. Each declared event with no initializer names a new object of its
// own, an element of a fixed-size array and one `new[N]` makes included (Table
// 6-7, LRM 7.5.1); reading an entry an array does not have answers null (Table
// 7-1). An event reached any of those ways is triggered and waited on, alone or
// in an event list (LRM 15.5.1, 15.5.2, 9.4.2.1).
package events;
  event shared[2];

  task automatic wait_on_second(output bit woke);
    @shared[1];
    woke = 1;
  endtask
endpackage

class Waiters;
  local event waiting[$];

  // Each caller waits on an object of its own, kept in the queue.
  task park();
    event mine;
    waiting.push_back(mine);
    @mine;
  endtask

  function void wake_first();
    event first = waiting.pop_front();
    -> first;
  endfunction

  function int count();
    return waiting.size();
  endfunction
endclass

class Carrier;
  event done;
endclass

module Top;
  typedef struct {
    int id;
    event done;
  } job_t;

  event fixed[3];
  event sized[];
  event by_name[string];
  job_t job;
  int parked_woken = 0;
  int fixed_woken = 0;
  int sized_woken = 0;
  int named_woken = 0;
  int member_woken = 0;
  int property_woken = 0;
  int argument_woken = 0;
  int result_woken = 0;
  int list_woken = 0;
  int mixed_woken = 0;
  bit package_woken = 0;
  int parked_left = -1;
  bit fixed_distinct = 0;
  bit sized_distinct = 0;
  bit sized_padding_fresh = 0;
  bit sized_shrunk_copied = 0;
  bit missing_is_null = 0;
  bit popped_empty_is_null = 0;
  bit property_distinct = 0;
  bit package_distinct = 0;
  event list_a, list_b;
  event mixed_event;
  logic clk = 0;

  function automatic event pick(event from);
    return from;
  endfunction

  task automatic wait_on(event e);
    @e;
    argument_woken++;
  endtask

  Waiters w = new;
  Carrier first = new;
  Carrier second = new;

  initial begin
    event empty[$];
    event kept[];
    event shrunk[];
    event got;
    event from_function;

    fixed_distinct = fixed[0] != fixed[1] && fixed[1] != fixed[2];
    sized = new[2];
    sized_distinct = sized[0] != sized[1] && sized[0] != null;
    kept = new[3](sized);
    sized_padding_fresh = kept[2] != null && kept[2] != kept[1] && kept[0] == sized[0];
    shrunk = new[1](sized);
    sized_shrunk_copied = shrunk.size() == 1 && shrunk[0] == sized[0];
    by_name["k"] = fixed[0];
    missing_is_null = by_name["missing"] == null;
    got = empty.pop_front();
    popped_empty_is_null = got == null;
    property_distinct = first.done != second.done;
    package_distinct = events::shared[0] != events::shared[1];
    from_function = pick(job.done);

    fork
      begin
        w.park();
        parked_woken++;
      end
      begin
        w.park();
        parked_woken++;
      end
      begin
        @fixed[1];
        fixed_woken++;
      end
      begin
        @sized[1];
        sized_woken++;
      end
      begin
        @by_name["k"];
        named_woken++;
      end
      begin
        @job.done;
        member_woken++;
      end
      begin
        @second.done;
        property_woken++;
      end
      wait_on(fixed[2]);
      begin
        @from_function;
        result_woken++;
      end
      begin
        @(list_a or list_b);
        list_woken++;
      end
      begin
        @(mixed_event or posedge clk);
        mixed_woken++;
      end
      events::wait_on_second(package_woken);
      begin
        #1;
        w.wake_first();
        -> fixed[1];
        -> sized[1];
        -> fixed[0];
        -> job.done;
        -> second.done;
        -> fixed[2];
        -> list_b;
        -> mixed_event;
        -> events::shared[1];
        #1;
        parked_left = w.count();
        w.wake_first();
      end
    join
  end

  final begin
    if (fixed_distinct !== 1) $fatal(1, "the elements of a fixed array of events did not name distinct objects");
    if (sized_distinct !== 1) $fatal(1, "the elements new[2] made did not name distinct objects");
    if (sized_padding_fresh !== 1) $fatal(1, "new[3](src) did not copy src's events and give the rest new ones");
    if (sized_shrunk_copied !== 1) $fatal(1, "new[1](src) did not keep only src's first event");
    if (missing_is_null !== 1) $fatal(1, "reading an entry an associative array lacks did not answer null");
    if (popped_empty_is_null !== 1) $fatal(1, "popping an empty queue of events did not answer null");
    if (property_distinct !== 1) $fatal(1, "two objects' event properties named one object");
    if (package_distinct !== 1) $fatal(1, "the elements of a package's array of events named one object");
    if (package_woken !== 1) $fatal(1, "a package task's wait on a package event did not wake");
    if (parked_woken !== 2) $fatal(1, "events kept in a queue woke %0d of 2 waiters", parked_woken);
    if (parked_left !== 1) $fatal(1, "the queue held %0d waiters after the first was woken", parked_left);
    if (fixed_woken !== 1) $fatal(1, "a fixed array element's wait woke %0d times", fixed_woken);
    if (sized_woken !== 1) $fatal(1, "a dynamic array element's wait woke %0d times", sized_woken);
    if (named_woken !== 1) $fatal(1, "an associative array element's wait woke %0d times", named_woken);
    if (member_woken !== 1) $fatal(1, "a structure member's wait woke %0d times", member_woken);
    if (property_woken !== 1) $fatal(1, "a class property's wait woke %0d times", property_woken);
    if (argument_woken !== 1) $fatal(1, "a wait on an argument woke %0d times", argument_woken);
    if (result_woken !== 1) $fatal(1, "a wait on a returned event woke %0d times", result_woken);
    if (list_woken !== 1) $fatal(1, "an event list of events woke %0d times", list_woken);
    if (mixed_woken !== 1) $fatal(1, "an event list mixing an event and an edge woke %0d times", mixed_woken);
    $display("All checks passed");
  end
endmodule
