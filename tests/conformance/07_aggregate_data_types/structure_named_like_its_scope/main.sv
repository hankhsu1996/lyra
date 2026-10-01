// A module's name belongs to the definitions name space and a package's to the
// package name space, while a structure type declared inside either belongs to
// that scope's own names (LRM 3.13), so a structure may be named exactly like
// the module or package declaring it. Each is still its own type, and every
// whole-value operation applies to it: assignment, equality member by member
// (LRM 7.2, 11.4.5).
package Top_pkg;
  typedef struct {
    int count;
    logic [3:0] tag;
  } Top_pkg;
endpackage

module Top;
  typedef struct {
    int value;
    string label;
  } Top;

  Top_pkg::Top_pkg from_package, package_copy;
  Top from_module, module_copy;
  logic package_equal;
  logic module_equal;

  initial begin
    from_package = '{3, 4'b1010};
    package_copy = from_package;
    package_equal = from_package == package_copy;

    from_module = '{9, "nine"};
    module_copy = from_module;
    module_copy.label = "other";
    module_equal = from_module == module_copy;
  end

  final begin
    if (package_equal !== 1'b1)
      $fatal(1, "a structure named like its package compared unequal to a copy");
    if (package_copy.count !== 3 || package_copy.tag !== 4'b1010)
      $fatal(1, "a structure named like its package was not copied whole");
    if (module_equal !== 1'b0)
      $fatal(1, "structures differing in one member compared equal");
    if (module_copy.value !== 9 || module_copy.label != "other")
      $fatal(1, "a structure named like its module was not written by member");
    $display("All checks passed");
  end
endmodule
