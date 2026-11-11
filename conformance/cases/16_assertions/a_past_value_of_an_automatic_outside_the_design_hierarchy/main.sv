// A past value of an automatic variable is its current value (LRM 16.5.1),
// so a sampled value function whose operand reads only automatic variables
// and constants reads the operand as it is now, wherever it is called
// -- in a subroutine of a package, and in a method of a class a package
// declares, as in a module.
package P;
  bit clk;

  task automatic past_of(int v, output int answer);
    answer = $past(v + 1, 1, , @(posedge clk));
  endtask

  class Reader;
    task stable_of(int v, output int answer);
      answer = $stable(v, @(posedge clk));
    endtask
  endclass
endpackage

module Top;
  typedef int ints_t[$];
  int answers[$];
  int stables[$];

  always #5 P::clk = !P::clk;

  initial begin
    static P::Reader reader = new;
    for (int n = 0; n < 3; n++) begin
      int answer;
      int stable;
      @(posedge P::clk);
      P::past_of(n * 10, answer);
      answers.push_back(answer);
      reader.stable_of(n, stable);
      stables.push_back(stable);
    end
    $finish;
  end

  final begin
    if (answers != ints_t'{1, 11, 21})
      $fatal(1, "the package task answered %p, expected '{1, 11, 21}", answers);
    if (stables != ints_t'{1, 1, 1})
      $fatal(1, "the class method answered %p, expected '{1, 1, 1}", stables);
    $display("All checks passed");
  end
endmodule
