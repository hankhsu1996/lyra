// What a call's operand read is what the call is handed, whatever an operand
// evaluated after it does to where it was read. An object such an operand
// reaches is in use until the call takes it, so a later operand dropping every
// other handle to it does not reclaim it first (LRM 8.29: "Users do not need
// to worry about dangling references, premature deallocation"; an object is
// reclaimed once it is "no longer being used"). The standard fixes no order
// among a call's arguments (LRM 13.5), so each position here has two answers:
// the operand is read before the later one replaces what it read, or after.
// Either way it is read whole, and never out of storage given back.
//
// The positions: the object a method is called on, reached through a property
// and through the last element of a queue; a handle and a string passed by
// value; a part of an object an operation is entered on; and a task enable's
// operands, whose call waits.
module Top;
  typedef class Obj;
  typedef class Holder;

  int task_answer;

  class Obj;
    int v;
    function int plus(int x);
      return v + x;
    endfunction
    task plus_later(int x);
      #1 task_answer = v + x;
    endtask
  endclass

  class Holder;
    Obj obj;
    Obj objs[$];
    string text;
  endclass

  Holder kept;
  // Long enough that a string holds it in storage of its own, so a reassignment
  // gives the storage of the value before it back.
  string named = "a value long enough to be kept apart from its holder";

  function automatic Obj made(int v);
    Obj o = new;
    o.v = v;
    return o;
  endfunction

  // A holder whose objects and text are the first ones, each held by nothing
  // else, so replacing them reclaims them.
  function automatic void fill();
    kept = new;
    kept.obj = made(41);
    kept.objs.push_back(made(41));
    kept.text = "first text long enough to be kept apart from its holder";
  endfunction

  function automatic int replace_obj();
    kept.obj = made(100);
    return 1;
  endfunction

  function automatic int replace_last();
    kept.objs[$] = made(100);
    return 1;
  endfunction

  function automatic int replace_holder();
    kept = new;
    kept.text = "other text long enough to be kept apart from its holder";
    return 4;
  endfunction

  function automatic int rename();
    named = "another value long enough to be kept apart from its holder";
    return 1;
  endfunction

  function automatic int value_of(Obj o, int x);
    return o.v + x;
  endfunction

  function automatic int length_of(string s, int x);
    return s.len() + x;
  endfunction

  task automatic value_later(Obj o, int x);
    #1 task_answer = o.v + x;
  endtask

  int through_property;
  int through_element;
  int as_argument;
  int string_length;
  string part;
  int task_argument;
  int task_receiver;

  initial begin
    fill();
    through_property = kept.obj.plus(replace_obj());
    fill();
    through_element = kept.objs[$].plus(replace_last());
    fill();
    as_argument = value_of(kept.obj, replace_obj());
    string_length = length_of(named, rename());
    fill();
    part = kept.text.substr(0, replace_holder());
    fill();
    value_later(kept.obj, replace_obj());
    task_argument = task_answer;
    fill();
    kept.obj.plus_later(replace_obj());
    task_receiver = task_answer;
  end

  function automatic void expect_either(string what, int got, int a, int b);
    if (got !== a && got !== b)
      $fatal(1, "%s was %0d, expected %0d or %0d", what, got, a, b);
  endfunction

  final begin
    expect_either("through_property", through_property, 42, 101);
    expect_either("through_element", through_element, 42, 101);
    expect_either("as_argument", as_argument, 42, 101);
    expect_either("string_length", string_length, 53, 59);
    if (part != "first" && part != "other")
      $fatal(1, "part was %s, expected first or other", part);
    expect_either("task_argument", task_argument, 42, 101);
    expect_either("task_receiver", task_receiver, 42, 101);
    $display("All checks passed");
  end
endmodule
