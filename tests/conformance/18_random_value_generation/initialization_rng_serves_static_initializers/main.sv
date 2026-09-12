// An initialization RNG belongs to every module instance and to every package,
// is seeded with the default seed, and is used in the creation of static
// initializers as well as static processes (LRM 18.14.1). A variable
// declaration assignment runs before any procedure starts (LRM 10.5, and LRM
// 26.2 for a package), so a randomization function called from one has a
// generator to draw from even though no process is executing. Two instances of
// one module draw alike, because each instance's generator starts from the same
// seed and neither advances the other's.

package Draws;
  int unsigned package_level = $urandom_range(1, 10);
endpackage

class Counter;
  static int unsigned class_level = $urandom_range(1, 10);
endclass

module Leaf #(
    parameter int unsigned Tag = 0
);
  int unsigned module_level = $urandom_range(1, 10);
  int unsigned raw = $urandom;
  int unsigned label = Tag;
endmodule

module Top;
  Leaf #(.Tag(1)) u1 ();
  Leaf #(.Tag(2)) u2 ();

  int unsigned block_level;
  int unsigned function_level;

  function automatic int unsigned Kept();
    static int unsigned once = $urandom_range(1, 10);
    return once;
  endfunction

  initial begin
    int unsigned declared = $urandom_range(1, 10);
    block_level   = declared;
    function_level = Kept();
  end

  final begin
    if (u1.module_level < 1 || u1.module_level > 10)
      $fatal(1, "a module-level initializer drew %0d, expected 1 to 10",
             u1.module_level);
    if (u2.module_level < 1 || u2.module_level > 10)
      $fatal(1, "the second instance drew %0d, expected 1 to 10",
             u2.module_level);
    if (u1.label !== 1 || u2.label !== 2)
      $fatal(1, "the two instances labelled themselves %0d and %0d",
             u1.label, u2.label);
    if (u1.module_level !== u2.module_level)
      $fatal(1, "two instances drew %0d and %0d, expected the same value",
             u1.module_level, u2.module_level);
    if (u1.raw !== u2.raw)
      $fatal(1, "two instances drew %0h and %0h unranged, expected the same",
             u1.raw, u2.raw);
    if (Draws::package_level < 1 || Draws::package_level > 10)
      $fatal(1, "a package-level initializer drew %0d, expected 1 to 10",
             Draws::package_level);
    if (Counter::class_level < 1 || Counter::class_level > 10)
      $fatal(1, "a static property initializer drew %0d, expected 1 to 10",
             Counter::class_level);
    if (block_level < 1 || block_level > 10)
      $fatal(1, "a static block declaration drew %0d, expected 1 to 10",
             block_level);
    if (function_level < 1 || function_level > 10)
      $fatal(1, "a static local of a subroutine drew %0d, expected 1 to 10",
             function_level);
    $display("All checks passed");
  end
endmodule
