// The sampled value of an automatic variable is its current value, and so is
// its past value when a sampled value function asks for one (LRM 16.5.1), so
// such a value is stable at every tick and never changed (LRM 16.9.3). A
// `ref` formal that is not `ref static` may be used only where an automatic
// variable may (LRM 13.5.2), so it is sampled the same way: what it holds now,
// not what its actual held in the Preponed region.
//
// Each edge the task samples, the variable it reads was changed earlier in the
// same time step, so the Preponed value would be one less than the current one.
module Top;
  typedef int samples_t[$];

  bit clk;
  int a;
  samples_t past_of_local;
  samples_t sampled_of_local;
  samples_t stable_of_local;
  samples_t changed_of_local;
  samples_t past_of_formal;
  samples_t sampled_of_formal;

  always #5 clk = !clk;

  task automatic sample_local();
    int x;
    repeat (2) begin
      @(posedge clk);
      #0;
      x = a;
      past_of_local.push_back($past(x, 1, , @(posedge clk)));
      sampled_of_local.push_back($sampled(x));
      stable_of_local.push_back($stable(x, @(posedge clk)));
      changed_of_local.push_back($changed(x, @(posedge clk)));
    end
  endtask

  task automatic sample_formal(ref int r);
    repeat (2) begin
      @(posedge clk);
      #0;
      past_of_formal.push_back($past(r, 1, , @(posedge clk)));
      sampled_of_formal.push_back($sampled(r));
    end
  endtask

  always @(posedge clk) a = a + 1;

  initial begin
    fork
      sample_local();
      sample_formal(a);
    join
    $finish;
  end

  final begin
    if (past_of_local != samples_t'{1, 2})
      $fatal(1, "$past of a local was %p, expected '{1, 2}", past_of_local);
    if (sampled_of_local != samples_t'{1, 2})
      $fatal(1, "$sampled of a local was %p, expected '{1, 2}",
             sampled_of_local);
    if (stable_of_local != samples_t'{1, 1})
      $fatal(1, "$stable of a local was %p, expected '{1, 1}", stable_of_local);
    if (changed_of_local != samples_t'{0, 0})
      $fatal(1, "$changed of a local was %p, expected '{0, 0}",
             changed_of_local);
    if (past_of_formal != samples_t'{1, 2})
      $fatal(1, "$past of a ref formal was %p, expected '{1, 2}",
             past_of_formal);
    if (sampled_of_formal != samples_t'{1, 2})
      $fatal(1, "$sampled of a ref formal was %p, expected '{1, 2}",
             sampled_of_formal);
    $display("All checks passed");
  end
endmodule
