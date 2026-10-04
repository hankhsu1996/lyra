// A hierarchical name reaches a handle of an interface class the declaring
// scope also declares (LRM 23.8, 23.9), and a method entered through it may be
// one an interface class it extends states rather than its own, since an
// interface class extending another inherits its methods (LRM 8.26.2). Either
// way the object's own class answers (LRM 8.20, 8.26), so a call through the
// handle runs the implementation the object was built with. The interface
// class extended may be declared beside the one extending it or by a package.
package sizes_pkg;
  interface class Sized;
    pure virtual function int Size(int extra);
  endclass
endpackage

module Inner;
  interface class Measured;
    pure virtual function int Area(int scale);
  endclass

  interface class Labelled extends Measured, sizes_pkg::Sized;
    pure virtual function int Label();
  endclass

  class Square implements Labelled;
    int side = 3;
    virtual function int Area(int scale);
      return side * side * scale;
    endfunction
    virtual function int Size(int extra);
      return side * 100 + extra;
    endfunction
    virtual function int Label();
      return 7;
    endfunction
  endclass

  Labelled held;

  initial begin
    Square s = new();
    held = s;
  end
endmodule

module Top;
  Inner inner ();

  int label = 0;
  int area = 0;
  int size = 0;

  initial begin
    #1;
    label = inner.held.Label();
    area = inner.held.Area(2);
    size = inner.held.Size(5);
  end

  final begin
    if (label !== 7)
      $fatal(1, "the behavior the handle's own class states answered %0d, expected 7", label);
    if (area !== 18)
      $fatal(1, "a behavior an extended interface class states answered %0d, expected 18", area);
    if (size !== 305)
      $fatal(1, "a behavior a package's interface class states answered %0d, expected 305", size);
    $display("All checks passed");
  end
endmodule
