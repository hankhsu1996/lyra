// The this keyword denotes a predefined object handle that refers to the object
// used to invoke the subroutine it appears in. It qualifies a class property, a
// value parameter, or a method of that instance: an unqualified name is
// resolved in the innermost scope, so a subroutine argument or a local
// declaration shadows a property of the same name and only the qualified form
// reaches the property, while where nothing is shadowed both forms name the
// same member, including one inherited from a base class and one the class owns
// rather than each instance. Used on its own the keyword is that handle, so it
// may be returned, passed, and compared for identity with a handle the caller
// holds; it also names the current instance's own type
// (LRM 8.11, 8.9, 8.13, 6.23).
module Top;
  class Demo #(int P = 7);
    static int shared = 4;
    int x;
    int y;

    function new(int x);
      this.x = x;
      y = 0;
    endfunction

    function int shared_value();
      return this.shared;
    endfunction

    function automatic int through_type_of_this();
      var type(this) other;
      other = new(8);
      return other.x;
    endfunction

    function void bump();
      this.y = this.y + 1;
    endfunction

    function void bump_twice();
      this.bump();
      bump();
    endfunction

    function int param_value();
      return this.P;
    endfunction

    function automatic int shadowed_by_local();
      int x = 100;
      return this.x + x;
    endfunction

    task automatic tick();
      #1 this.y = this.y + 100;
    endtask
  endclass

  class Initialized;
    int base_value = 3;
    int derived_value = this.base_value * 2;
  endclass

  class Node;
    function Node itself();
      return this;
    endfunction

    function bit names_same_object(Node other);
      return other == this;
    endfunction
  endclass

  class Derived extends Demo #(2);
    function new();
      super.new(4);
    endfunction

    function void raise();
      this.y = 9;
    endfunction
  endclass

  int qualified_argument = -1;
  int after_bump = -1;
  int after_bump_twice = -1;
  int parameter_through_this = -1;
  int local_shadows_property = -1;
  int after_suspending_task = -1;
  int inherited_through_this = -1;
  int derived_parameter = -1;
  int initializer_through_this = -1;
  int static_through_this = -1;
  int type_of_this = -1;
  bit returned_this_is_same = 0;
  bit passed_this_is_same = 0;
  bit other_object_differs = 1;

  initial begin
    Demo #(3) d;
    Derived e;
    Initialized f;
    Node g;
    Node h;

    d = new(5);
    qualified_argument = d.x;

    d.bump();
    after_bump = d.y;

    d.bump_twice();
    after_bump_twice = d.y;

    parameter_through_this = d.param_value();
    local_shadows_property = d.shadowed_by_local();
    static_through_this = d.shared_value();
    type_of_this = d.through_type_of_this();

    d.tick();
    after_suspending_task = d.y;

    e = new();
    e.raise();
    inherited_through_this = e.y;
    derived_parameter = e.param_value();

    f = new();
    initializer_through_this = f.derived_value;

    g = new();
    h = new();
    returned_this_is_same = (g.itself() == g);
    passed_this_is_same = g.names_same_object(g);
    other_object_differs = g.names_same_object(h);
  end

  final begin
    if (qualified_argument !== 5)
      $fatal(1, "qualified_argument was %0d, expected 5", qualified_argument);
    if (after_bump !== 1)
      $fatal(1, "after_bump was %0d, expected 1", after_bump);
    if (after_bump_twice !== 3)
      $fatal(1, "after_bump_twice was %0d, expected 3", after_bump_twice);
    if (parameter_through_this !== 3)
      $fatal(1, "parameter_through_this was %0d, expected 3",
             parameter_through_this);
    if (local_shadows_property !== 105)
      $fatal(1, "local_shadows_property was %0d, expected 105",
             local_shadows_property);
    if (after_suspending_task !== 103)
      $fatal(1, "after_suspending_task was %0d, expected 103",
             after_suspending_task);
    if (inherited_through_this !== 9)
      $fatal(1, "inherited_through_this was %0d, expected 9",
             inherited_through_this);
    if (derived_parameter !== 2)
      $fatal(1, "derived_parameter was %0d, expected 2", derived_parameter);
    if (initializer_through_this !== 6)
      $fatal(1, "initializer_through_this was %0d, expected 6",
             initializer_through_this);
    if (static_through_this !== 4)
      $fatal(1, "static_through_this was %0d, expected 4", static_through_this);
    if (type_of_this !== 8)
      $fatal(1, "type_of_this was %0d, expected 8", type_of_this);
    if (returned_this_is_same !== 1)
      $fatal(1, "returned_this_is_same was %0d, expected 1",
             returned_this_is_same);
    if (passed_this_is_same !== 1)
      $fatal(1, "passed_this_is_same was %0d, expected 1", passed_this_is_same);
    if (other_object_differs !== 0)
      $fatal(1, "other_object_differs was %0d, expected 0",
             other_object_differs);
    $display("All checks passed");
  end
endmodule
