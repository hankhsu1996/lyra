// A class declared in a package is a type of the whole system, so a body in
// another scope may name it without ever naming one of its properties: build an
// object of it, hold that object as a class it extends or as an interface class
// it implements, and get the object back as the class it was built as with
// `$cast`. Every one of those is the same object, so a virtual method called
// through any view runs the body of the class the object was built as (LRM 8.4,
// 8.13, 8.16, 8.20, 8.26, 26.3).
package shapes_pkg;
  interface class Measured;
    pure virtual function int measure();
  endclass

  class Base implements Measured;
    int side;

    function new(int s);
      side = s;
    endfunction

    virtual function int measure();
      return side;
    endfunction
  endclass

  class Square extends Base;
    function new(int s);
      super.new(s);
    endfunction

    virtual function int measure();
      return side * side;
    endfunction
  endclass
endpackage

module Top;
  // Takes the object as the interface class, so the caller hands over a view
  // it formed from a class it names nowhere else.
  function automatic int measured_through(shapes_pkg::Measured m);
    return m.measure();
  endfunction

  int through_interface = -1;
  int through_base = -1;
  int through_argument = -1;
  bit cast_succeeded = 0;
  bit same_object = 0;
  bit wrong_cast_refused = 0;

  initial begin
    shapes_pkg::Square built;
    shapes_pkg::Base as_base;
    shapes_pkg::Measured as_interface;
    shapes_pkg::Square recovered;
    shapes_pkg::Base plain;
    shapes_pkg::Square not_a_square;

    built = new(3);
    as_base = built;
    as_interface = built;
    through_interface = as_interface.measure();
    through_base = as_base.measure();
    through_argument = measured_through(built);

    cast_succeeded = $cast(recovered, as_interface);
    same_object = (recovered == built);

    // An object built as the base is not a value of the class extending it,
    // so the cast fails and leaves its destination as it was.
    plain = new(4);
    not_a_square = built;
    wrong_cast_refused = !$cast(not_a_square, plain) && not_a_square == built;
  end

  final begin
    if (through_interface !== 9)
      $fatal(1, "through_interface was %0d, expected 9", through_interface);
    if (through_base !== 9)
      $fatal(1, "through_base was %0d, expected 9", through_base);
    if (through_argument !== 9)
      $fatal(1, "through_argument was %0d, expected 9", through_argument);
    if (cast_succeeded !== 1)
      $fatal(1, "the cast back to the class the object was built as failed");
    if (same_object !== 1)
      $fatal(1, "the cast answered a different object than the one built");
    if (wrong_cast_refused !== 1)
      $fatal(1, "a cast to a class the object is not of succeeded");
    $display("All checks passed");
  end
endmodule
