// A wait on an expression that calls a subroutine ends when the value the call
// answers changes (LRM 9.4.2): a non-edge event is any change in the value of
// the expression, and a change to an object member, an aggregate element or a
// dynamic array's size that a method or function reads reevaluates it. So what
// the call reads counts wherever it lies -- a variable of the module, an
// element, a static property, a variable of the instance a virtual interface
// holds, a member of the object the method runs on or of one a handle reaches,
// and what a subroutine it calls reads in turn -- and a wait on it is not
// limited to the call's arguments. It holds however the function is reached:
// a method or a static method, a package function, an interface's function
// through a virtual interface, one named hierarchically, and a virtual method,
// which reads what the implementation that runs reads. The same holds of
// `wait (cond)`.
//
// Which object a method reads is the one the call is made on when the wait
// looks, so writing the handle moves the wait to the new object, and a write
// to the object it named before no longer reaches it. A function that reaches
// an object or a virtual interface only through a variable of its own still
// wakes the wait when what it read there changes, and so does an array
// method's `with` clause it hands each element to; a handle it guards against
// null is not followed while it is null. A static variable the function
// declares exists before any call runs, so another call changing it is a
// change the wait sees.
//
// Every call in the function is followed the same way. A variable handed by
// `ref` to a function that reads it is read (LRM 13.5.2). An object the
// function constructs reads, in its constructor, in its properties'
// initializers, in what an extends specifier hands its base and in its base's
// constructor, what any function would. A
// system function that answers from the current size of a dynamically sized
// value reads that value, written in the waited expression or in the function
// alike (LRM 20.6.2, 20.7). Learning what the function reads is the evaluation
// the wait makes anyway, so a system function with an effect inside it -- one
// that draws from a seed -- and a function a constructor calls run once per
// evaluation and no more. Nothing but the evaluation refers to an object the
// function makes and reads, and once it is gone nothing can write it, so a wait
// whose value comes from such an object waits on: its going is no change to
// what the wait reads (LRM 8.29, 9.4.2).
package counters;
  int count = 0;
  int built_from = 0;
  int initialized_from = 0;
  int base_built_from = 0;
  int clause_from = 0;
  int counted_calls = 0;
  int fresh_calls = 0;

  function automatic int current();
    return count;
  endfunction

  function automatic int counted();
    counted_calls++;
    return count;
  endfunction
endpackage

class CallsInItsConstructor;
  int got;

  function new();
    got = counters::counted();
    got = got + 0;
  endfunction
endclass

class Built;
  int from_body;
  int from_initializer = counters::initialized_from;

  function new();
    from_body = counters::built_from;
  endfunction
endclass

class BuiltOnBase;
  int from_base;

  function new();
    from_base = counters::base_built_from;
  endfunction
endclass

class Derived extends BuiltOnBase;
  function new();
    super.new();
  endfunction
endclass

class TakesOne;
  int got;

  function new(int x);
    got = x;
  endfunction
endclass

class HandedOnTheClause extends TakesOne(counters::clause_from);
endclass

interface Bus;
  int d;
  int e;

  function int read_e();
    return e;
  endfunction
endinterface

module Holder;
  int held = 0;

  function int read_held();
    return held;
  endfunction
endmodule

class Base;
  int level = 0;

  virtual function int read_level();
    return level;
  endfunction
endclass

class Doubled extends Base;
  virtual function int read_level();
    return 2 * level;
  endfunction
endclass

class Node;
  static int shared = 0;
  int status = 0;
  Node next;

  static function int read_shared();
    return shared;
  endfunction

  function int get();
    return status;
  endfunction

  function int next_status();
    return next.status;
  endfunction

  task automatic AwaitOwnStatus(output time woke);
    @(get());
    woke = $time;
  endtask
endclass

module Top;
  int g = 0;
  int a = 0;
  int arr[4];
  Node gh;
  Node p;
  Node other;
  Node chain;
  Node holder;
  Node list;
  Node self_owner;
  Node guarded;
  Node rebound;
  Node level;
  virtual Bus vb;
  Bus bus ();
  Holder holder_inst ();
  Base dispatched;

  time plain_woke = 0;
  time argument_woke = 0;
  time level_plain_woke = 0;
  time element_woke = 0;
  time static_property_woke = 0;
  time static_method_woke = 0;
  time global_handle_woke = 0;
  time interface_woke = 0;
  time transitive_woke = 0;
  time package_woke = 0;
  time method_woke = 0;
  time method_chain_woke = 0;
  time handle_formal_woke = 0;
  time own_method_woke = 0;
  time walked_woke = 0;
  time recursive_woke = 0;
  time guarded_woke = 0;
  time rebound_woke = 0;
  time level_method_woke = 0;
  time interface_function_woke = 0;
  time hierarchical_woke = 0;
  time virtual_woke = 0;
  time own_static_woke = 0;
  time by_ref_woke = 0;
  time constructor_body_woke = 0;
  time initializer_woke = 0;
  time base_constructor_woke = 0;
  time size_woke = 0;
  time size_in_function_woke = 0;
  time bits_woke = 0;
  time high_in_function_woke = 0;
  time drawing_woke = 0;
  time interface_local_woke = 0;
  time with_clause_woke = 0;
  time base_clause_woke = 0;
  time fresh_woke = 0;

  Node bag[$];

  int by_ref_target = 0;
  int sized[];
  byte stream[$];
  int seed = 3;

  function automatic int read_through_ref(ref int r);
    return r;
  endfunction

  function automatic int passes_by_ref();
    return read_through_ref(by_ref_target);
  endfunction

  function automatic int built_body();
    Built b = new;
    return b.from_body;
  endfunction

  function automatic int built_initializer();
    Built b = new;
    return b.from_initializer;
  endfunction

  function automatic int built_base();
    Derived d = new;
    return d.from_base;
  endfunction

  function automatic int built_on_clause();
    HandedOnTheClause h = new;
    return h.got;
  endfunction

  function int size_of_sized();
    return $size(sized);
  endfunction

  function int high_of_stream();
    return $high(stream);
  endfunction

  function int plus_g(int x);
    return x - x + g;
  endfunction

  function int draws();
    return plus_g($random(seed));
  endfunction

  function automatic int fresh_status();
    Node made = new;
    counters::fresh_calls++;
    return made.status;
  endfunction

  function automatic int built_calling();
    CallsInItsConstructor c = new;
    return c.got;
  endfunction

  function int reads_g();
    return g;
  endfunction

  function int reads_g_plus(int x);
    return x + g;
  endfunction

  function int reads_element(int i);
    return arr[i];
  endfunction

  function int reads_static_property();
    return Node::shared;
  endfunction

  function int reads_global_handle();
    return gh.status;
  endfunction

  function int reads_interface();
    return vb.d;
  endfunction

  function automatic int reads_interface_held_locally();
    virtual Bus held = vb;
    return held.d;
  endfunction

  function int bag_total();
    return bag.sum() with (item.status);
  endfunction

  function int reads_through_another();
    return reads_static_property();
  endfunction

  function automatic int status_of(Node n);
    return n.status;
  endfunction

  function automatic int sum_walked(Node head);
    int total = 0;
    for (Node n = head; n != null; n = n.next) total += n.status;
    return total;
  endfunction

  function automatic int sum_recursive(Node n);
    return n == null ? 0 : n.status + sum_recursive(n.next);
  endfunction

  function automatic int status_or_zero(Node n);
    return n == null ? 0 : n.status;
  endfunction

  function automatic int tally(bit bump);
    static int count;
    if (bump) count = count + 1;
    return count;
  endfunction

  initial begin
    gh = new;
    p = new;
    other = new;
    other.status = 8;
    chain = new;
    chain.next = new;
    holder = new;
    list = new;
    list.next = new;
    self_owner = new;
    rebound = new;
    level = new;
    vb = bus;
    begin
      automatic Node first = new;
      automatic Node second = new;
      automatic Doubled d = new;
      bag.push_back(first);
      bag.push_back(second);
      dispatched = d;
    end
    fork
      begin @(reads_g()); plain_woke = $time; end
      begin @(reads_g_plus(a)); argument_woke = $time; end
      begin wait (reads_g() == 1); level_plain_woke = $time; end
      begin @(reads_element(2)); element_woke = $time; end
      begin @(reads_static_property()); static_property_woke = $time; end
      begin @(Node::read_shared()); static_method_woke = $time; end
      begin @(reads_global_handle()); global_handle_woke = $time; end
      begin @(reads_interface()); interface_woke = $time; end
      begin @(reads_interface_held_locally()); interface_local_woke = $time; end
      begin @(bag_total()); with_clause_woke = $time; end
      begin @(built_on_clause()); base_clause_woke = $time; end
      begin @(reads_through_another()); transitive_woke = $time; end
      begin @(counters::current()); package_woke = $time; end
      begin @(p.get()); method_woke = $time; end
      begin @(chain.next_status()); method_chain_woke = $time; end
      begin @(status_of(holder)); handle_formal_woke = $time; end
      self_owner.AwaitOwnStatus(own_method_woke);
      begin @(sum_walked(list)); walked_woke = $time; end
      begin @(sum_recursive(list)); recursive_woke = $time; end
      begin @(status_or_zero(guarded)); guarded_woke = $time; end
      begin @(rebound.get()); rebound_woke = $time; end
      begin wait (level.get() == 2); level_method_woke = $time; end
      begin @(vb.read_e()); interface_function_woke = $time; end
      begin @(holder_inst.read_held()); hierarchical_woke = $time; end
      begin @(dispatched.read_level()); virtual_woke = $time; end
      begin @(tally(0)); own_static_woke = $time; end
      begin @(passes_by_ref()); by_ref_woke = $time; end
      begin @(built_body()); constructor_body_woke = $time; end
      begin @(built_initializer()); initializer_woke = $time; end
      begin @(built_base()); base_constructor_woke = $time; end
      begin @($size(sized)); size_woke = $time; end
      begin @(size_of_sized()); size_in_function_woke = $time; end
      begin wait ($bits(stream) > 8); bits_woke = $time; end
      begin @(high_of_stream()); high_in_function_woke = $time; end
      begin wait (fresh_status() == 1); fresh_woke = $time; end
    join_none
    #5 g = 1;
    #5 arr[2] = 3;
    #5 Node::shared = 1;
    #5 gh.status = 4;
    #5 bus.d = 5;
    #5 counters::count = 1;
    #5 p.status = 6;
    // The handle a member holds is written first, which moves the wait to the
    // new object without changing the answer, and then that object's member.
    #5 chain.next = new;
    #5 chain.next.status = 7;
    #5 holder.status = 1;
    #5 self_owner.status = 2;
    // A member two steps down the list, reached only through a local.
    #5 list.next.status = 3;
    // The guarded handle is set to an object whose member already differs.
    #5 begin
      guarded = new;
      guarded.status = 9;
    end
    // Writing the handle to an object whose member already differs is an
    // event; the old object is not watched any more.
    #5 rebound = other;
    #5 level.status = 1;
    #5 level.status = 2;
    #5 bus.e = 1;
    #5 holder_inst.held = 1;
    #5 dispatched.level = 1;
    #5 void'(tally(1));
    #5 by_ref_target = 1;
    #5 counters::built_from = 1;
    #5 counters::initialized_from = 1;
    #5 counters::base_built_from = 1;
    #5 sized = new[2];
    #5 stream.push_back(8'd1);
    #5 stream.push_back(8'd2);
    #5 fork
      begin @(draws()); drawing_woke = $time; end
      begin @(built_calling()); end
    join_none
    #5 g = 2;
    #5 bag[1].status = 4;
    #5 counters::clause_from = 1;
  end

  final begin
    if (plain_woke !== 5) $fatal(1, "a function reading a module variable ended the wait at %0t, expected 5", plain_woke);
    if (argument_woke !== 5) $fatal(1, "a function reading an argument and a module variable ended the wait at %0t, expected 5", argument_woke);
    if (level_plain_woke !== 5) $fatal(1, "a level wait on a function ended at %0t, expected 5", level_plain_woke);
    if (element_woke !== 10) $fatal(1, "a function reading an element ended the wait at %0t, expected 10", element_woke);
    if (static_property_woke !== 15) $fatal(1, "a function reading a static property ended the wait at %0t, expected 15", static_property_woke);
    if (static_method_woke !== 15) $fatal(1, "a static method ended the wait at %0t, expected 15", static_method_woke);
    if (transitive_woke !== 15) $fatal(1, "a function calling another ended the wait at %0t, expected 15", transitive_woke);
    if (global_handle_woke !== 20) $fatal(1, "a function reading through a module handle ended the wait at %0t, expected 20", global_handle_woke);
    if (interface_woke !== 25) $fatal(1, "a function reading through a virtual interface ended the wait at %0t, expected 25", interface_woke);
    if (interface_local_woke !== 25) $fatal(1, "a function reading through a virtual interface it holds itself ended the wait at %0t, expected 25", interface_local_woke);
    if (base_clause_woke !== 155) $fatal(1, "what an extends specifier hands the base ended the wait at %0t, expected 155", base_clause_woke);
    if (with_clause_woke !== 150) $fatal(1, "a function's array method reading through each element ended the wait at %0t, expected 150", with_clause_woke);
    if (package_woke !== 30) $fatal(1, "a package function ended the wait at %0t, expected 30", package_woke);
    if (method_woke !== 35) $fatal(1, "a method ended the wait at %0t, expected 35", method_woke);
    if (method_chain_woke !== 45) $fatal(1, "a method reading through a member handle ended the wait at %0t, expected 45", method_chain_woke);
    if (handle_formal_woke !== 50) $fatal(1, "a function reading through a handle argument ended the wait at %0t, expected 50", handle_formal_woke);
    if (own_method_woke !== 55) $fatal(1, "a method called on its own object ended the wait at %0t, expected 55", own_method_woke);
    if (walked_woke !== 60) $fatal(1, "a function walking a list ended the wait at %0t, expected 60", walked_woke);
    if (recursive_woke !== 60) $fatal(1, "a recursive function ended the wait at %0t, expected 60", recursive_woke);
    if (guarded_woke !== 65) $fatal(1, "a function guarding a null handle ended the wait at %0t, expected 65", guarded_woke);
    if (rebound_woke !== 70) $fatal(1, "a method on a rewritten handle ended the wait at %0t, expected 70", rebound_woke);
    if (level_method_woke !== 80) $fatal(1, "a level wait on a method ended at %0t, expected 80", level_method_woke);
    if (interface_function_woke !== 85) $fatal(1, "an interface's function called through a virtual interface ended the wait at %0t, expected 85", interface_function_woke);
    if (hierarchical_woke !== 90) $fatal(1, "a function called by a hierarchical name ended the wait at %0t, expected 90", hierarchical_woke);
    if (virtual_woke !== 95) $fatal(1, "a virtual method ended the wait at %0t, expected 95", virtual_woke);
    if (own_static_woke !== 100) $fatal(1, "a function's own static variable ended the wait at %0t, expected 100", own_static_woke);
    if (by_ref_woke !== 105) $fatal(1, "a variable handed by 'ref' ended the wait at %0t, expected 105", by_ref_woke);
    if (constructor_body_woke !== 110) $fatal(1, "what a constructor reads ended the wait at %0t, expected 110", constructor_body_woke);
    if (initializer_woke !== 115) $fatal(1, "what a property's initializer reads ended the wait at %0t, expected 115", initializer_woke);
    if (base_constructor_woke !== 120) $fatal(1, "what a base's constructor reads ended the wait at %0t, expected 120", base_constructor_woke);
    if (size_woke !== 125) $fatal(1, "the size of a dynamic array ended the wait at %0t, expected 125", size_woke);
    if (size_in_function_woke !== 125) $fatal(1, "the size of a dynamic array a function reads ended the wait at %0t, expected 125", size_in_function_woke);
    if (high_in_function_woke !== 130) $fatal(1, "the top index of a queue a function reads ended the wait at %0t, expected 130", high_in_function_woke);
    if (bits_woke !== 135) $fatal(1, "a level wait on the bits a queue holds ended at %0t, expected 135", bits_woke);
    if (drawing_woke !== 145) $fatal(1, "a function drawing from a seed ended the wait at %0t, expected 145", drawing_woke);
    begin
      automatic int reference = 3;
      automatic int drawn;
      drawn = $random(reference);
      drawn = $random(reference);
      if (seed !== reference) $fatal(1, "a wait and one change drew from the seed more than twice");
    end
    if (counters::counted_calls !== 1)
      $fatal(1, "a function a constructor calls ran %0d times in one evaluation, expected 1",
             counters::counted_calls);
    if (fresh_woke !== 0)
      $fatal(1, "a wait on an object only its evaluation made ended at %0t", fresh_woke);
    if (counters::fresh_calls !== 1)
      $fatal(1, "a wait on an object only its evaluation made evaluated %0d times, expected 1",
             counters::fresh_calls);
    $display("All checks passed");
  end
endmodule
