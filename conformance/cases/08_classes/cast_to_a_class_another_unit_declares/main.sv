// $cast toward a class asks whether the object the source refers to is one a
// variable of that class may hold, which the object's own class answers (LRM
// 8.16). Nothing about the destination class is read but which class it is, so
// a cast toward a class that this module only names -- one another unit
// declares, whose objects this module never creates and whose members it never
// reaches -- answers the same as a cast toward one it uses in full. The class
// may be named as written, or through a type parameter of a parameterized
// class another unit declares (LRM 8.25), which a specialization stands in for
// where it is written.
class Base;
endclass

class Derived extends Base;
endclass

class Other extends Base;
endclass

package P;
  class Shape;
    int sides = 0;
  endclass

  class Square extends Shape;
    int length = 7;
  endclass

  class Registry #(type T = int);
    static function T get(Shape s);
      T t;
      if (!$cast(t, s)) t = null;
      return t;
    endfunction
  endclass
endpackage

module Holds;
  Derived kept;
  P::Square square;
endmodule

module Top;
  Holds holds ();

  Base base_object = new;
  Other other_object = new;
  int answer_from_a_base_object;
  int answer_from_another_subclass;
  int answer_kept_as_it_was;
  int length_through_the_specialization;
  int answer_from_a_shape;

  initial begin
    Derived d;
    P::Square found;
    P::Square square;
    P::Shape shape;

    answer_from_a_base_object = $cast(d, base_object);
    holds.kept = null;
    answer_from_another_subclass = $cast(holds.kept, other_object);
    answer_kept_as_it_was = (holds.kept == null);

    square = new;
    shape = square;
    found = P::Registry#(P::Square)::get(shape);
    length_through_the_specialization = found.length;
    shape = new;
    answer_from_a_shape = $cast(holds.square, shape);

    if (answer_from_a_base_object !== 0)
      $fatal(1, "answer_from_a_base_object was %0d, expected 0",
             answer_from_a_base_object);
    if (answer_from_another_subclass !== 0)
      $fatal(1, "answer_from_another_subclass was %0d, expected 0",
             answer_from_another_subclass);
    if (answer_kept_as_it_was !== 1)
      $fatal(1, "answer_kept_as_it_was was %0d, expected 1",
             answer_kept_as_it_was);
    if (length_through_the_specialization !== 7)
      $fatal(1, "length_through_the_specialization was %0d, expected 7",
             length_through_the_specialization);
    if (answer_from_a_shape !== 0)
      $fatal(1, "answer_from_a_shape was %0d, expected 0", answer_from_a_shape);
    $display("All checks passed");
  end
endmodule
