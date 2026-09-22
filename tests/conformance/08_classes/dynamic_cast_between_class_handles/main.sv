// Assigning a subclass handle to a superclass variable is always legal, while
// the reverse is illegal to write directly and is what $cast exists for: it
// succeeds when the object the source refers to is one the destination variable
// may hold, which is decided by the object rather than by the source's declared
// type, so the same downcast succeeds or fails depending on what was
// constructed. It also succeeds when the destination is the same class or a
// superclass of the source, and for the literal null; a source that refers to
// no object at all satisfies no run-time check. An interface class handle is a
// legal source for the same check. An invalid assignment leaves the destination
// unchanged and the function form answers 0 without issuing an error
// (LRM 8.16, 6.24.2, 8.26).
module Top;
  interface class Drivable;
    pure virtual function int wheels();
  endclass

  class Vehicle;
    int weight;

    function new(int weight);
      this.weight = weight;
    endfunction
  endclass

  class Car extends Vehicle implements Drivable;
    int doors;

    function new(int weight, int doors);
      super.new(weight);
      this.doors = doors;
    endfunction

    virtual function int wheels();
      return 4;
    endfunction
  endclass

  class RaceCar extends Car;
    int top_speed;

    function new(int weight, int doors, int top_speed);
      super.new(weight, doors);
      this.top_speed = top_speed;
    endfunction
  endclass

  int answer_for_upcast;
  int weight_after_upcast;
  int answer_for_downcast;
  int doors_after_downcast;
  int answer_for_wrong_class;
  int doors_after_wrong_class;
  int answer_across_two_levels;
  int top_speed_after_two_levels;
  int answer_for_null_source;
  int doors_after_null_source;
  int answer_for_literal_null;
  int answer_from_interface_source;
  int doors_from_interface_source;
  int answer_from_task_spelling;
  int doors_from_task_spelling;

  initial begin
    Vehicle v;
    Car c;
    Car held;
    RaceCar r;
    Drivable d;

    // The destination is a superclass of the source: legal whatever the object.
    c = new(1200, 4);
    weight_after_upcast = 0;
    answer_for_upcast = $cast(v, c);
    weight_after_upcast = v.weight;

    // The destination is a subclass, and the object really is one.
    held = new(1300, 2);
    v = held;
    c = null;
    doors_after_downcast = 0;
    answer_for_downcast = $cast(c, v);
    doors_after_downcast = c.doors;

    // The same downcast where the object is only the superclass: the
    // destination keeps what it held.
    v = new(900);
    c = held;
    answer_for_wrong_class = $cast(c, v);
    doors_after_wrong_class = c.doors;

    // Two levels of inheritance between the source and the destination.
    r = new(1100, 2, 300);
    v = r;
    r = null;
    top_speed_after_two_levels = 0;
    answer_across_two_levels = $cast(r, v);
    top_speed_after_two_levels = r.top_speed;

    // A source that refers to no object satisfies no run-time check.
    v = null;
    c = held;
    answer_for_null_source = $cast(c, v);
    doors_after_null_source = c.doors;

    // The literal null is admissible on its own.
    c = held;
    answer_for_literal_null = $cast(c, null);

    // An interface class handle as the source.
    d = held;
    c = null;
    doors_from_interface_source = 0;
    answer_from_interface_source = $cast(c, d);
    doors_from_interface_source = c.doors;

    // The task spelling performs the same assignment.
    v = held;
    c = null;
    doors_from_task_spelling = 0;
    $cast(c, v);
    doors_from_task_spelling = c.doors;
    answer_from_task_spelling = (c == held) ? 1 : 0;
  end

  final begin
    if (answer_for_upcast !== 1)
      $fatal(1, "answer_for_upcast was %0d, expected 1", answer_for_upcast);
    if (weight_after_upcast !== 1200)
      $fatal(1, "weight_after_upcast was %0d, expected 1200",
             weight_after_upcast);
    if (answer_for_downcast !== 1)
      $fatal(1, "answer_for_downcast was %0d, expected 1", answer_for_downcast);
    if (doors_after_downcast !== 2)
      $fatal(1, "doors_after_downcast was %0d, expected 2",
             doors_after_downcast);
    if (answer_for_wrong_class !== 0)
      $fatal(1, "answer_for_wrong_class was %0d, expected 0",
             answer_for_wrong_class);
    if (doors_after_wrong_class !== 2)
      $fatal(1, "doors_after_wrong_class was %0d, expected 2",
             doors_after_wrong_class);
    if (answer_across_two_levels !== 1)
      $fatal(1, "answer_across_two_levels was %0d, expected 1",
             answer_across_two_levels);
    if (top_speed_after_two_levels !== 300)
      $fatal(1, "top_speed_after_two_levels was %0d, expected 300",
             top_speed_after_two_levels);
    if (answer_for_null_source !== 0)
      $fatal(1, "answer_for_null_source was %0d, expected 0",
             answer_for_null_source);
    if (doors_after_null_source !== 2)
      $fatal(1, "doors_after_null_source was %0d, expected 2",
             doors_after_null_source);
    if (answer_for_literal_null !== 1)
      $fatal(1, "answer_for_literal_null was %0d, expected 1",
             answer_for_literal_null);
    if (answer_from_interface_source !== 1)
      $fatal(1, "answer_from_interface_source was %0d, expected 1",
             answer_from_interface_source);
    if (doors_from_interface_source !== 2)
      $fatal(1, "doors_from_interface_source was %0d, expected 2",
             doors_from_interface_source);
    if (doors_from_task_spelling !== 2)
      $fatal(1, "doors_from_task_spelling was %0d, expected 2",
             doors_from_task_spelling);
    if (answer_from_task_spelling !== 1)
      $fatal(1, "answer_from_task_spelling was %0d, expected 1",
             answer_from_task_spelling);
    $display("All checks passed");
  end
endmodule
