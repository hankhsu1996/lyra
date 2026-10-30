// A handle names an object, so two handles to one object are equal however each
// was declared. LRM 8.14 makes a subclass object a legal value of a base-class
// variable and LRM 8.26.5 makes it a legal value of an interface class variable
// it implements -- both are views of one object, and neither may name a
// different one.
//
// The case states each view against a class that conforms to no interface and
// against one that does, and with a lineage that declares a virtual method and
// one that does not, because a target language is free to lay those out
// differently and a handle must not inherit the difference. What `this` answers
// is the same question asked from inside a body, and it is stated where the
// rest of that keyword is (08_classes/this_names_the_current_instance).
module Top;
  interface class Drivable;
    pure virtual function int Level();
  endclass

  class Plain;
    int tag;
    function new(int t);
      tag = t;
    endfunction
  endclass

  class PlainHeir extends Plain;
    function new(int t);
      super.new(t);
    endfunction
  endclass

  class PlainConforming extends Plain implements Drivable;
    function new(int t);
      super.new(t);
    endfunction
    virtual function int Level();
      return tag;
    endfunction
  endclass

  class Virtualized;
    int tag;
    function new(int t);
      tag = t;
    endfunction
    virtual function int Level();
      return tag;
    endfunction
  endclass

  class Conforming extends Virtualized implements Drivable;
    function new(int t);
      super.new(t);
    endfunction
  endclass

  initial begin
    automatic PlainHeir heir = new(1);
    automatic Plain as_plain = heir;
    automatic PlainConforming plain_conforming = new(2);
    automatic Drivable plain_as_drivable = plain_conforming;
    automatic Plain plain_conforming_as_plain = plain_conforming;
    automatic Conforming conforming = new(3);
    automatic Virtualized as_virtualized = conforming;
    automatic Drivable as_drivable = conforming;
    automatic PlainHeir other_heir = new(1);

    // A base view, with and without a virtual method in the lineage.
    if (as_plain != heir) $fatal(1, "a base view named another object");
    if (as_virtualized != conforming)
      $fatal(1, "a base view of a conforming class named another object");
    if (plain_conforming_as_plain != plain_conforming)
      $fatal(1, "a base view of a conforming class named another object");

    // A contract view. The lineage declaring no virtual method of its own is
    // the shape a target language is most likely to lay out differently.
    if (plain_as_drivable != plain_conforming)
      $fatal(1, "a contract view named another object");
    if (as_drivable != conforming)
      $fatal(1, "a contract view of a virtual lineage named another object");

    // Equality is which object, so two objects of one class stay distinct and a
    // view is not what makes them so.
    if (heir == other_heir) $fatal(1, "two objects compared equal");
    if (as_plain == Plain'(other_heir))
      $fatal(1, "base views of two objects compared equal");

    if (heir == null) $fatal(1, "a handle to an object compared equal to null");
    as_plain = null;
    if (as_plain != null) $fatal(1, "a nulled handle did not compare to null");

    $display("All checks passed");
  end
endmodule
