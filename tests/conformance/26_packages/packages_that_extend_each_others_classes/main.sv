// Two packages may each declare a class extending a class the other declares
// (LRM 8.13, 26.3). A class declaration is not a use of the other package's
// body, so nothing here depends on which package is elaborated first; each
// object carries the property its own class inherited, and reaching it names
// the class that declares it (LRM 8.14).
//
// A property a base keeps `local` is still storage of every object extending it
// (LRM 8.18 restricts who may name it, not whether it exists), so a class
// extending the base from another unit holds its own properties beside that
// storage without being able to name it. The lineage here crosses three units,
// and every level writes and reads back its own properties and the ones it
// inherited.
package first_pkg;
  class FirstBase;
    local int first_hidden = 11;
    int first_value = 1;

    function int first_hidden_value();
      return first_hidden;
    endfunction

    function void set_first_hidden(int v);
      first_hidden = v;
    endfunction
  endclass

  class FirstDerived extends second_pkg::SecondBase;
    int first_own = 3;
  endclass
endpackage

package second_pkg;
  class SecondBase;
    int second_value = 2;
    local int second_hidden = 22;

    function int second_hidden_value();
      return second_hidden;
    endfunction
  endclass

  class SecondDerived extends first_pkg::FirstBase;
    local int second_derived_hidden = 33;
    int second_own = 4;

    function int second_derived_hidden_value();
      return second_derived_hidden;
    endfunction
  endclass
endpackage

module Top;
  class Leaf extends second_pkg::SecondDerived;
    int leaf_own = 5;
  endclass

  int reached_second;
  int reached_first;
  int first_own;
  int second_hidden;
  int second_own;
  int first_hidden;
  int second_derived_hidden;
  int leaf_first_value;
  int leaf_first_hidden;
  int leaf_second_own;
  int leaf_second_derived_hidden;
  int leaf_own;

  initial begin
    first_pkg::FirstDerived a;
    second_pkg::SecondDerived b;
    Leaf c;

    a = new;
    b = new;
    c = new;
    reached_second = a.second_value;
    first_own = a.first_own;
    second_hidden = a.second_hidden_value();
    reached_first = b.first_value;
    second_own = b.second_own;
    first_hidden = b.first_hidden_value();
    second_derived_hidden = b.second_derived_hidden_value();

    c.first_value = 101;
    c.set_first_hidden(111);
    c.second_own = 104;
    c.leaf_own = 105;
    leaf_first_value = c.first_value;
    leaf_first_hidden = c.first_hidden_value();
    leaf_second_own = c.second_own;
    leaf_second_derived_hidden = c.second_derived_hidden_value();
    leaf_own = c.leaf_own;
  end

  final begin
    if (reached_second !== 2)
      $fatal(1, "reached_second was %0d, expected 2", reached_second);
    if (first_own !== 3) $fatal(1, "first_own was %0d, expected 3", first_own);
    if (second_hidden !== 22)
      $fatal(1, "second_hidden was %0d, expected 22", second_hidden);
    if (reached_first !== 1)
      $fatal(1, "reached_first was %0d, expected 1", reached_first);
    if (second_own !== 4)
      $fatal(1, "second_own was %0d, expected 4", second_own);
    if (first_hidden !== 11)
      $fatal(1, "first_hidden was %0d, expected 11", first_hidden);
    if (second_derived_hidden !== 33)
      $fatal(
          1, "second_derived_hidden was %0d, expected 33",
          second_derived_hidden);
    if (leaf_first_value !== 101)
      $fatal(1, "leaf_first_value was %0d, expected 101", leaf_first_value);
    if (leaf_first_hidden !== 111)
      $fatal(1, "leaf_first_hidden was %0d, expected 111", leaf_first_hidden);
    if (leaf_second_own !== 104)
      $fatal(1, "leaf_second_own was %0d, expected 104", leaf_second_own);
    if (leaf_second_derived_hidden !== 33)
      $fatal(
          1, "leaf_second_derived_hidden was %0d, expected 33",
          leaf_second_derived_hidden);
    if (leaf_own !== 105)
      $fatal(1, "leaf_own was %0d, expected 105", leaf_own);
    $display("All checks passed");
  end
endmodule
