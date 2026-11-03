// An import declaration is a subroutine of the scope it sits in (LRM 35.5.4),
// and a default it gives a formal is evaluated in that scope each time a call
// omits the argument (LRM 13.5.3). So a call made from another instance, by
// the import's hierarchical name, reads the declaring instance's variable,
// each instance its own, as a call from the instance itself and a default an
// import in a package gives do. A default of an `output` names the variable
// the foreign side's answer is written back to. A context import observes the
// scope its declaration sits in rather than its call site (LRM 35.5.3), so a
// subroutine that scope exports, called back from the foreign side, reads the
// declaring instance's variable too. An implicit list holds what a default
// reads, of a call of its own scope, and a name of a formal before it stands for
// what the call binds that formal (LRM 9.2.2.2.1). An open array takes no part
// in a default's evaluation and may stand before a formal given one.
package P;
  int base = 3;
  import "DPI-C" function int twice_of_package(input int x = base);
endpackage

module Leaf #(parameter int START = 0);
  int mine = START;
  int filled = -1;

  import "DPI-C" function int twice(input int x = mine);
  import "DPI-C" context function int through_export(input int x = mine);
  import "DPI-C" function void fill(output int x = filled);

  export "DPI-C" function own_mine;
  function automatic int own_mine();
    return mine;
  endfunction

  function automatic int own_call();
    return twice();
  endfunction

  import "DPI-C" function int add(input int a, input int b = a + mine);
  int first = 1;
  int doubled;
  int added;
  always_comb doubled = twice();
  always_comb added = add(first);
endmodule

module Top;
  Leaf #(.START(5)) left ();
  Leaf #(.START(8)) right ();

  import "DPI-C" function int sum_of(input int a[], input int n = 2);
  import "DPI-C" function int rows_plus(input int a[1:2][], input int n = 7);
  int values[4] = '{1, 2, 3, 4};
  int grid[1:2][3];

  initial begin
    left.mine = 6;
    right.mine = 9;
    if (left.twice() !== 12)
      $fatal(1, "left's default gave %0d, expected 12", left.twice());
    if (right.twice() !== 18)
      $fatal(1, "right's default gave %0d, expected 18", right.twice());
    if (left.twice(4) !== 8)
      $fatal(1, "a passed argument gave %0d, expected 8", left.twice(4));
    if (right.own_call() !== 18)
      $fatal(1, "a call from the instance gave %0d", right.own_call());
    if (left.through_export() !== 606)
      $fatal(1, "left's context import gave %0d, expected 606",
             left.through_export());
    if (right.through_export() !== 909)
      $fatal(1, "right's context import gave %0d, expected 909",
             right.through_export());
    right.fill();
    if (right.filled !== 77)
      $fatal(1, "an output default holds %0d, expected 77", right.filled);
    if (left.filled !== -1)
      $fatal(1, "another instance's output holds %0d", left.filled);
    if (sum_of(values) !== 3)
      $fatal(1, "a default after an open array gave %0d, expected 3",
             sum_of(values));
    if (rows_plus(grid) !== 9)
      $fatal(1, "a default after a partly open array gave %0d, expected 9",
             rows_plus(grid));
    left.mine = 10;
    left.first = 2;
    #1;
    if (left.doubled !== 20)
      $fatal(1, "a list held no read of a default, %0d", left.doubled);
    if (left.added !== 14)
      $fatal(1, "a default naming a formal gave %0d, expected 14", left.added);
    P::base = 4;
    if (P::twice_of_package() !== 8)
      $fatal(1, "a package import's default gave %0d, expected 8",
             P::twice_of_package());
    $display("All checks passed");
  end
endmodule
