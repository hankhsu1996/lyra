// An interface class declared inside a module is a type of each instance of
// the module, unique to that instance (LRM 6.22), so a dynamic cast toward it
// succeeds only for an object whose class implements that instance's interface
// class (LRM 8.16, 8.26.5) -- whether the class names it, extends a class that
// does (LRM 8.26), or implements an interface class extending it (LRM 8.26.2),
// and whichever scope of the instance declares the class. An interface class of
// a package is one type, which every instance's class implements alike.
package pkg;
  class Base;
  endclass

  interface class Named;
    pure virtual function int tag();
  endclass
endpackage

module M;
  interface class Shape;
    pure virtual function int sides();
  endclass

  interface class NamedShape extends pkg::Named, Shape;
  endclass

  class Square extends pkg::Base implements Shape, pkg::Named;
    virtual function int sides();
      return 4;
    endfunction
    virtual function int tag();
      return 1;
    endfunction
  endclass

  class Cube extends Square;
  endclass

  class Tri extends pkg::Base implements NamedShape;
    virtual function int sides();
      return 3;
    endfunction
    virtual function int tag();
      return 2;
    endfunction
  endclass

  class Plain extends pkg::Base implements pkg::Named;
    virtual function int tag();
      return 3;
    endfunction
  endclass

  for (genvar i = 0; i < 1; i++) begin : g
    class Blocky extends pkg::Base implements Shape;
      virtual function int sides();
        return 5;
      endfunction
    endclass
    Blocky b;
    initial b = new;
  end

  Square sq;
  Cube cu;
  Tri tr;
  Plain pl;
  initial begin
    sq = new;
    cu = new;
    tr = new;
    pl = new;
  end

  function automatic int is_shape(pkg::Base b);
    Shape s;
    return $cast(s, b);
  endfunction
  function automatic int is_named_shape(pkg::Base b);
    NamedShape s;
    return $cast(s, b);
  endfunction
  function automatic int named_is_shape(pkg::Named n);
    Shape s;
    return $cast(s, n);
  endfunction
  function automatic int is_named(pkg::Base b);
    pkg::Named n;
    return $cast(n, b);
  endfunction
  function automatic int sides_of(pkg::Base b);
    Shape s;
    if (!$cast(s, b)) return -1;
    return s.sides();
  endfunction
endmodule

module Top;
  M m1();
  M m2();

  initial begin
    #1;
    if (m1.is_shape(m1.sq) !== 1) $fatal(1, "its own instance's class");
    if (m1.is_shape(m2.sq) !== 0) $fatal(1, "another instance's class");
    if (m2.is_shape(m1.sq) !== 0) $fatal(1, "another instance's class");
    if (m1.is_shape(m1.cu) !== 1) $fatal(1, "a class extending its own");
    if (m1.is_shape(m2.cu) !== 0)
      $fatal(1, "a class extending another instance's");
    if (m1.is_shape(m1.g[0].b) !== 1)
      $fatal(1, "a class its generate block declares");
    if (m2.is_shape(m1.g[0].b) !== 0)
      $fatal(1, "a class another instance's generate block declares");
    if (m1.is_shape(m1.pl) !== 0) $fatal(1, "a class implementing none of it");
    if (m1.is_shape(m1.tr) !== 1)
      $fatal(1, "a class implementing an interface class extending it");
    if (m2.is_shape(m1.tr) !== 0)
      $fatal(1, "another instance's class reaching it through an extension");
    if (m1.is_named_shape(m1.tr) !== 1) $fatal(1, "its own extending one");
    if (m1.is_named_shape(m2.tr) !== 0)
      $fatal(1, "another instance's extending one");
    if (m1.is_named_shape(m1.sq) !== 0)
      $fatal(1, "a class implementing what it extends, not it");
    if (m1.named_is_shape(m1.sq) !== 1)
      $fatal(1, "from an interface class handle, its own instance's");
    if (m1.named_is_shape(m2.sq) !== 0)
      $fatal(1, "from an interface class handle, another instance's");
    if (m1.is_named(m2.sq) !== 1) $fatal(1, "a package's interface class");
    if (m2.is_named(m1.tr) !== 1)
      $fatal(1, "a package's interface class through an extension");
    if (m1.sides_of(m1.cu) !== 4) $fatal(1, "a call after the cast");
    if (m1.sides_of(m2.cu) !== -1) $fatal(1, "a failed cast leaves no call");
    $display("All checks passed");
  end
endmodule
