// The operands of an operator are all evaluated (LRM 11.3.5) in an order the
// standard does not state, the expressions of an argument list in an order it
// calls undefined (LRM 13.5), and a write's target and value likewise where no
// timing control separates them (LRM 10.4.1, 10.4.2). Lyra runs them first to
// last, as written, with a write's value before its target -- the order a
// write with an intra-assignment timing control takes -- and a call's
// arguments before the object it is made on; a read takes what its storage
// holds when it runs. This case pins that choice wherever an operand that
// changes storage stands beside one that reads or changes it: two calls of one
// operator, a read and a call writing what it read, the arguments of a
// function, of a task, of a method and of an interface class's method, a call
// whose argument moves the handle the call is made on, an indexed write whose
// index and value both call, blocking, nonblocking and delayed, and compound,
// whose value reads its own target and so runs the index first (LRM 11.4.1), a
// read of an element of an array a function holds beside a write of it, a
// write of a local, or a call writing it through a reference, before a read
// of it, a reference bound past a queue's end beside a read of its length, a
// method made on a value another call made, whose argument writes what that
// call reads, and values too wide for a frame; and that a choice and a loop
// evaluate each operand as often as they run it.
module Top;
  interface class Taker;
    pure virtual function int take(int a, int b);
  endclass

  class Counter implements Taker;
    int seen;
    int id;
    function new(int id);
      this.id = id;
    endfunction
    virtual function int take(int a, int b);
      seen = a * 10 + b;
      return id;
    endfunction
  endclass

  string trace;
  int shared_value;
  int slots [4];
  Counter holder;
  Counter other;

  function automatic int note(string name, int answer);
    trace = {trace, name};
    shared_value = 100;
    return answer;
  endfunction

  function automatic int pair(int a, int b);
    return a * 10 + b;
  endfunction

  function automatic int move_holder(int answer);
    trace = {trace, "m"};
    holder = other;
    return answer;
  endfunction

  function automatic logic [65535:0] wide(string name, int answer);
    trace = {trace, name};
    return 65536'(answer);
  endfunction

  task automatic waits_for(int a, int b);
    #1;
  endtask

  // A read of an element of an array the frame holds, then a write of it.
  function automatic int read_then_write_element();
    int held [2];
    held[0] = 1;
    return held[0] + (held[0] = 5);
  endfunction

  // A write of a local the frame holds, then a read of it.
  function automatic int write_then_read_local();
    int held;
    held = 1;
    return (held = 5) + held;
  endfunction

  // Writes the variable a reference it is handed names.
  function automatic int set_through(ref int target);
    target = 7;
    return 0;
  endfunction

  // A call writing a local through a reference, then a read of the local.
  function automatic int call_then_read_local();
    int held;
    held = 1;
    return set_through(held) + held;
  endfunction

  string text = "hello";

  // Writes the string the call a method is made on reads.
  function automatic int capitalize();
    trace = {trace, "c"};
    text = "HELLO";
    return 1;
  endfunction

  function automatic string mark(string name);
    trace = {trace, name};
    return "";
  endfunction

  int queued [$];
  int length_seen;

  // Binding a reference to the position past a queue's end appends to it.
  task automatic bind_then_count(ref int element, input int length);
    length_seen = length;
  endtask

  string two_calls;
  int read_then_call;
  int call_then_read;
  string function_arguments;
  string task_arguments;
  string method_arguments;
  string interface_arguments;
  int moved_handle_answer;
  string index_and_value;
  int written_slot;
  string nonblocking;
  string compound;
  int compound_slot;
  string delayed;
  string choice;
  string looped;
  string wide_values;
  logic [65535:0] wide_answer;
  int element_read_then_written;
  int local_written_then_read;
  int local_called_then_read;
  string method_on_a_made_value;
  string made_then_argued;

  initial begin
    Counter counter;
    Taker taker;
    int sum;

    counter = new(1);
    taker = counter;
    holder = new(2);
    other = new(3);

    trace = "";
    sum = note("a", 1) + note("b", 2);
    two_calls = trace;

    shared_value = 1;
    read_then_call = shared_value + note("c", 5);

    shared_value = 1;
    call_then_read = note("d", 5) + shared_value;

    trace = "";
    sum = pair(note("e", 1), note("f", 2));
    function_arguments = trace;

    trace = "";
    waits_for(note("g", 1), note("h", 2));
    task_arguments = trace;

    trace = "";
    sum = counter.take(note("i", 1), note("j", 2));
    method_arguments = trace;

    trace = "";
    sum = taker.take(note("k", 1), note("l", 2));
    interface_arguments = trace;

    moved_handle_answer = holder.take(move_holder(1), 2);

    trace = "";
    slots[note("n", 2)] = note("o", 7);
    index_and_value = trace;
    written_slot = slots[2];

    trace = "";
    slots[note("p", 0)] <= note("q", 9);
    nonblocking = trace;

    trace = "";
    slots[1] = 4;
    slots[note("r", 1)] += note("s", 3);
    compound = trace;
    compound_slot = slots[1];

    trace = "";
    slots[note("t", 3)] = #1 note("u", 5);
    delayed = trace;

    trace = "";
    sum = note("v", 1) > 0 ? note("w", 2) : note("x", 3);
    choice = trace;

    trace = "";
    for (int i = 0; i < 3; i++) sum += note("y", i);
    looped = trace;

    trace = "";
    wide_answer = wide("1", 6) ^ wide("2", 3);
    wide_values = trace;

    element_read_then_written = read_then_write_element();
    local_written_then_read = write_then_read_local();
    local_called_then_read = call_then_read_local();

    trace = "";
    method_on_a_made_value =
        {text.substr(0, 3).substr(capitalize(), 2), mark("d")};
    made_then_argued = trace;

    bind_then_count(queued[queued.size()], queued.size());
  end

  final begin
    if (two_calls != "ab")
      $fatal(1, "two calls of one operator ran as '%s', expected 'ab'",
             two_calls);
    if (read_then_call != 6)
      $fatal(1, "a read before a call that writes it gave %0d, expected 6",
             read_then_call);
    if (call_then_read != 105)
      $fatal(1, "a read after a call that writes it gave %0d, expected 105",
             call_then_read);
    if (function_arguments != "ef")
      $fatal(1, "a function's arguments ran as '%s', expected 'ef'",
             function_arguments);
    if (task_arguments != "gh")
      $fatal(1, "a task's arguments ran as '%s', expected 'gh'",
             task_arguments);
    if (method_arguments != "ij")
      $fatal(1, "a method's arguments ran as '%s', expected 'ij'",
             method_arguments);
    if (interface_arguments != "kl")
      $fatal(1, "an interface class method's arguments ran as '%s', expected 'kl'",
             interface_arguments);
    if (moved_handle_answer != 3)
      $fatal(1, "a call whose argument moved its handle was made on object %0d, expected 3",
             moved_handle_answer);
    if (index_and_value != "on")
      $fatal(1, "an indexed write ran as '%s', expected 'on'",
             index_and_value);
    if (written_slot != 7)
      $fatal(1, "the indexed write stored %0d, expected 7", written_slot);
    if (nonblocking != "qp")
      $fatal(1, "a nonblocking indexed write ran as '%s', expected 'qp'",
             nonblocking);
    if (slots[0] != 9)
      $fatal(1, "the nonblocking write stored %0d, expected 9", slots[0]);
    if (compound != "rs")
      $fatal(1, "a compound indexed write ran as '%s', expected 'rs'",
             compound);
    if (compound_slot != 7)
      $fatal(1, "the compound write stored %0d, expected 7", compound_slot);
    if (delayed != "ut")
      $fatal(1, "a delayed indexed write ran as '%s', expected 'ut'",
             delayed);
    if (slots[3] != 5)
      $fatal(1, "the delayed write stored %0d, expected 5", slots[3]);
    if (choice != "vw")
      $fatal(1, "a choice ran as '%s', expected 'vw'", choice);
    if (looped != "yyy")
      $fatal(1, "a loop's operand ran as '%s', expected 'yyy'", looped);
    if (wide_values != "12")
      $fatal(1, "two values too wide for a frame ran as '%s', expected '12'",
             wide_values);
    if (wide_answer != 65536'(5))
      $fatal(1, "the wide values combined wrongly");
    if (element_read_then_written != 6)
      $fatal(1, "a read of an element before a write of it gave %0d, expected 6",
             element_read_then_written);
    if (local_written_then_read != 10)
      $fatal(1, "a write of a local before a read of it gave %0d, expected 10",
             local_written_then_read);
    if (local_called_then_read != 7)
      $fatal(1, "a call writing a local before a read of it gave %0d, expected 7",
             local_called_then_read);
    if (method_on_a_made_value != "EL")
      $fatal(1, "a method made on a value another call made gave '%s', expected 'EL'",
             method_on_a_made_value);
    if (made_then_argued != "cd")
      $fatal(1, "a method's argument and a later operand ran as '%s', expected 'cd'",
             made_then_argued);
    if (length_seen != 1)
      $fatal(1, "a length read after binding past a queue's end gave %0d, expected 1",
             length_seen);
    $display("All checks passed");
  end
endmodule
