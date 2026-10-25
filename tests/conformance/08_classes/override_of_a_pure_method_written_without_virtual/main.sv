// A method overriding a virtual method is virtual whether or not it repeats
// the keyword (LRM 8.20), and that holds for an override of a pure virtual
// prototype (LRM 8.21) and of one declared `extern` (LRM 8.24) alike. So a
// class extending it may override it again, and a call through a handle of
// the class that left out the keyword runs the object's own -- here with the
// classes in a package and the class overriding them again in a module.
package shapes;
  virtual class Shape;
    pure virtual function int rank();
    pure virtual function int width();
  endclass

  class Square extends Shape;
    function int rank();
      return 1;
    endfunction

    extern function int width();
  endclass

  function int Square::width();
    return 10;
  endfunction
endpackage

module Top;
  class Tile extends shapes::Square;
    function int rank();
      return 2;
    endfunction

    function int width();
      return 20;
    endfunction
  endclass

  int square_rank = -1;
  int square_width = -1;
  int own_rank = -1;
  int own_width = -1;

  initial begin
    shapes::Square square;
    Tile tile;
    tile = new;
    square = tile;
    square_rank = square.rank();
    square_width = square.width();
    square = new;
    own_rank = square.rank();
    own_width = square.width();
  end

  final begin
    if (square_rank !== 2)
      $fatal(1, "rank through the package class's handle was %0d, expected 2",
             square_rank);
    if (square_width !== 20)
      $fatal(1, "width through the package class's handle was %0d, expected 20",
             square_width);
    if (own_rank !== 1)
      $fatal(1, "rank of the package class's own object was %0d, expected 1",
             own_rank);
    if (own_width !== 10)
      $fatal(1, "width of the package class's own object was %0d, expected 10",
             own_width);
    $display("All checks passed");
  end
endmodule
