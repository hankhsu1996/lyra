// What Lyra answers of a scope where IEEE 1800 leaves the answer to the tool.
//
// - A scope the source did not name -- a block declaring a variable, a `for`
//   loop declaring its index (LRM 37.12 details 1, 2) -- answers no name and
//   no full name. A scope named inside one answers its name, and as its full
//   name the names of the named scopes around it with none for the unnamed
//   one, which is what `%m` prints inside it (LRM 21.2.1.5); no hierarchical
//   name reaches it (LRM 9.3.4), and a full name is not one that has to (LRM
//   37.10 detail 5 lets one be ambiguous).
// - The delay mode and decay time directives are informative (LRM Annex E) and
//   Lyra reads none, so a module answers no delay mode and an undefined decay
//   time, without an error.
// - Lyra reads no configuration (LRM 33.4), so a module answers no
//   configuration, the cell its text lies in as its cell -- a module declared
//   inside another lies in that one's, as `%l` prints it (LRM 33.7) -- and
//   `work`, the library of what the command line names, as its library.
// - A scope whose identifier is escaped answers its name as a hierarchical
//   name writes it, the backslash and the space ending it included (LRM
//   5.6.1, 23.6), a subroutine imported from C and a clocking block as much
//   as an instance.
// - A package is reached by its name alone, as no module shares it.
// - The built-in package `std` is no object: its classes are Lyra's own
//   definitions of what the front end declares.
// - `vpiInternalScope` from a module answers every scope directly inside it:
//   the instances and generate scopes among them, each element of an array
//   or a loop counted on its own, and no array itself.
// - A relation whose objects Lyra does not yet reach, and a property no scope
//   has, answer nothing and say so through `vpi_chk_error` (LRM 38.2).
package P;
endpackage

module Leaf;
endmodule

module Outer;
  module Inner;
  endmodule
  Inner inner ();
endmodule

module Top;
  import "DPI-C" context function int ask();
  import "DPI-C" walk_plus = function int \walk+ ();

  Leaf one ();
  Outer outer ();
  Leaf pair[2] ();
  for (genvar i = 0; i < 2; i++) begin : g
  end
  wire w;
  logic clk;
  clocking \cb+ @(posedge clk);
  endclocking

  int differences = -1;
  string nested_printed;
  string in_for_printed;

  initial begin
    begin
      int unnamed;
      begin : nested
        nested_printed = $sformatf("%m");
      end
    end
    for (int i = 0; i < 1; i++) begin : in_for
      in_for_printed = $sformatf("%m");
    end
    differences = ask();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the questions found %0d differences", differences);
    if (nested_printed != "Top.nested")
      $fatal(1, "%%m in nested printed '%s'", nested_printed);
    if (in_for_printed != "Top.in_for")
      $fatal(1, "%%m in in_for printed '%s'", in_for_printed);
    $display("All checks passed");
  end
endmodule
