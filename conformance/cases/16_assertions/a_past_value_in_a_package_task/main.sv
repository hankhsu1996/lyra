// A sampled value function may be called in procedural code (LRM 16.9.3), a
// task of a package among it, where the clock it counts is the one it names.
// Its operand is a variable of the package, so it is that variable's sampled
// value at the most recent strictly prior tick.
package P;
  bit clk;
  int g = 0;
  int seen[$];

  task automatic read_one();
    @(posedge clk);
    #1 seen.push_back($past(g, 1, , @(posedge clk)));
  endtask
endpackage

module Top;
  typedef int ints_t[$];
  always #5 P::clk = !P::clk;
  always @(posedge P::clk) P::g <= P::g + 1;

  initial begin
    repeat (3) P::read_one();
    $finish;
  end

  // The ticks at 5, 15 and 25 sample g as 0, 1 and 2, and each read stands a
  // step after its tick, so the most recent strictly prior tick is that one.
  final begin
    if (P::seen != ints_t'{0, 1, 2})
      $fatal(1, "the task read %p, expected '{0, 1, 2}", P::seen);
    $display("All checks passed");
  end
endmodule
