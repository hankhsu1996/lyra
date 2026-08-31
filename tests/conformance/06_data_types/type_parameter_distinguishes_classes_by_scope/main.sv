// A class is a type identified by its declaration, not by its contents (LRM
// 8.3), so two classes declared in two packages are two types however alike
// they are written -- and a package's name space is what tells them apart (LRM
// 26.3). A module parameterized by a type therefore gets a different type from
// each, and a method called through the parameter reaches the class that was
// actually bound. Two instantiations bound to the same class get the same one,
// so telling them apart is not merely a matter of counting instantiations.
package Slow;
  class Engine;
    function int rate();
      return 1;
    endfunction
  endclass
endpackage

package Fast;
  class Engine;
    function int rate();
      return 2;
    endfunction
  endclass
endpackage

module Driver #(
    parameter type T = int
);
  int seen = -1;

  initial begin
    T engine;
    engine = new();
    seen   = engine.rate();
  end
endmodule

module Top;
  Driver #(Slow::Engine) slow ();
  Driver #(Fast::Engine) fast ();
  Driver #(Slow::Engine) also_slow ();

  final begin
    if (slow.seen !== 1) $fatal(1, "slow.seen was %0d, expected 1", slow.seen);
    if (fast.seen !== 2) $fatal(1, "fast.seen was %0d, expected 2", fast.seen);
    if (also_slow.seen !== 1)
      $fatal(1, "also_slow.seen was %0d, expected 1", also_slow.seen);
    $display("All checks passed");
  end
endmodule
