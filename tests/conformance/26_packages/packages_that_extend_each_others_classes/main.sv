// Two packages may each declare a class extending a class the other declares
// (LRM 8.13, 26.3). A class declaration is not a use of the other package's
// body, so nothing here depends on which package is elaborated first; each
// object carries the property its own class inherited, and reaching it names
// the class that declares it (LRM 8.14).
package first_pkg;
  class FirstBase;
    int first_value = 1;
  endclass

  class FirstDerived extends second_pkg::SecondBase;
  endclass
endpackage

package second_pkg;
  class SecondBase;
    int second_value = 2;
  endclass

  class SecondDerived extends first_pkg::FirstBase;
  endclass
endpackage

module Top;
  int reached_second;
  int reached_first;

  initial begin
    first_pkg::FirstDerived a;
    second_pkg::SecondDerived b;

    a = new;
    b = new;
    reached_second = a.second_value;
    reached_first  = b.first_value;
  end

  final begin
    if (reached_second !== 2)
      $fatal(1, "reached_second was %0d, expected 2", reached_second);
    if (reached_first !== 1)
      $fatal(1, "reached_first was %0d, expected 1", reached_first);
    $display("All checks passed");
  end
endmodule
