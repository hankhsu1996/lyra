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
// an object only through a variable of its own still wakes the wait when that
// object changes, and a handle it guards against null is not followed while
// it is null. A static variable the function declares exists before any call
// runs, so another call changing it is a change the wait sees.
package counters;
  int count = 0;

  function automatic int current();
    return count;
  endfunction
endpackage

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
    static int count = 0;
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
      Doubled d = new;
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
    $display("All checks passed");
  end
endmodule
