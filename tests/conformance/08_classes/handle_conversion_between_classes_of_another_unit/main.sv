// A handle of a derived class is a handle of its base and of every interface
// class it implements (LRM 8.13, 8.26), and `$cast` turns it back where the
// object is of the target class (LRM 8.16). That holds where every class
// involved is declared in a package and the conversion is written in a module
// that reads nothing of the derived class but through it: a handle passed,
// returned and assigned as its base, compared with a handle of the base, held
// as an interface class, and cast down.
package shapes;
  interface class Ranked;
    pure virtual function int rank();
  endclass

  class Shape;
    int sides = 0;
  endclass

  class Square extends Shape implements Ranked;
    function new();
      sides = 4;
    endfunction

    virtual function int rank();
      return 2;
    endfunction
  endclass

  function automatic Square make_square();
    Square s = new;
    return s;
  endfunction

  function automatic Shape make_shape_of_square();
    Square s = new;
    return s;
  endfunction
endpackage

module Top;
  function automatic shapes::Shape as_shape(shapes::Square s);
    return s;
  endfunction

  function automatic bit same(shapes::Shape a, shapes::Square b);
    return a == b;
  endfunction

  function automatic int rank_of(shapes::Square s);
    shapes::Ranked r = s;
    return r.rank();
  endfunction

  function automatic int cast_down(shapes::Shape s, output int sides);
    shapes::Square square;
    sides = -1;
    if (!$cast(square, s)) return 0;
    sides = square.sides;
    return 1;
  endfunction

  int returned_sides = -1;
  int assigned_sides = -1;
  bit compared = 0;
  int ranked = -1;
  int cast_succeeded = -1;
  int cast_sides = -1;

  initial begin
    shapes::Square square;
    shapes::Shape shape;
    square = shapes::make_square();
    returned_sides = as_shape(square).sides;
    shape = square;
    assigned_sides = shape.sides;
    compared = same(shape, square);
    ranked = rank_of(square);
    cast_succeeded = cast_down(shapes::make_shape_of_square(), cast_sides);
  end

  final begin
    if (returned_sides !== 4)
      $fatal(1, "sides through a returned base handle were %0d, expected 4",
             returned_sides);
    if (assigned_sides !== 4)
      $fatal(1, "sides through an assigned base handle were %0d, expected 4",
             assigned_sides);
    if (compared !== 1)
      $fatal(1, "a base handle and the derived handle it was assigned from compared unequal");
    if (ranked !== 2)
      $fatal(1, "rank through an interface class handle was %0d, expected 2", ranked);
    if (cast_succeeded !== 1 || cast_sides !== 4)
      $fatal(1, "casting down gave %0d with %0d sides, expected 1 with 4",
             cast_succeeded, cast_sides);
    $display("All checks passed");
  end
endmodule
