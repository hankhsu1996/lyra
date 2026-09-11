// An identifier is a sequence of letters, digits, dollar signs, and
// underscores, or an escaped identifier carrying any printable character other
// than white space; the leading backslash and the terminating white space are
// not part of the name, so an escaped identifier denotes exactly the characters
// between them. Two declarations whose names differ are therefore two
// declarations however those characters are spelled, including when one name
// contains the period that separates the parts of a qualified reference, when
// two names differ only in a dollar sign where another has an underscore, and
// when a method is named for something the language spells with a keyword
// (LRM 5.6, 5.6.1, 8.7, 26.3).
package \p.A ;
  class B;
    static int n = 2;
  endclass
endpackage

package p;
  class \A.B ;
    static int n = 1;
  endclass

  class a$b;
    static int n = 3;
  endclass

  class a_b;
    static int n = 4;
  endclass

  class C;
    static int made = 0;

    function new();
      made = made + 1;
    endfunction

    function int \constructor ();
      return 7;
    endfunction
  endclass
endpackage

module Top;
  int period_in_a_class_name;
  int period_in_a_package_name;
  int dollar_sign_name;
  int underscore_name;
  int through_a_method_named_for_a_construct;
  int objects_constructed;

  initial begin
    p::C c;

    period_in_a_class_name = p::\A.B ::n;
    period_in_a_package_name = \p.A ::B::n;
    dollar_sign_name = p::a$b::n;
    underscore_name = p::a_b::n;

    c = new;
    through_a_method_named_for_a_construct = c.\constructor ();
    objects_constructed = p::C::made;
  end

  final begin
    // Each value is distinct, so two declarations that collapsed into one
    // would answer with the other's value rather than with nothing.
    if (period_in_a_class_name !== 1)
      $fatal(1, "period_in_a_class_name was %0d, expected 1",
             period_in_a_class_name);
    if (period_in_a_package_name !== 2)
      $fatal(1, "period_in_a_package_name was %0d, expected 2",
             period_in_a_package_name);
    if (dollar_sign_name !== 3)
      $fatal(1, "dollar_sign_name was %0d, expected 3", dollar_sign_name);
    if (underscore_name !== 4)
      $fatal(1, "underscore_name was %0d, expected 4", underscore_name);
    // A method may carry the name of a construct the language spells another
    // way; calling it reaches that method and constructs nothing.
    if (through_a_method_named_for_a_construct !== 7)
      $fatal(1, "through_a_method_named_for_a_construct was %0d, expected 7",
             through_a_method_named_for_a_construct);
    if (objects_constructed !== 1)
      $fatal(1, "objects_constructed was %0d, expected 1", objects_constructed);
    $display("All checks passed");
  end
endmodule
