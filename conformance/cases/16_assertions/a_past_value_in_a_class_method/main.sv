// A sampled value function may be called in procedural code (LRM 16.9.3), a
// method of a class among it, where the clock it counts is the one it names.
// Its operand here is a variable of the module declaring the class, so it is
// that variable's sampled value at the most recent strictly prior tick.
module Top;
  typedef int ints_t[$];
  bit clk;
  int g = 0;
  int seen[$];

  always #5 clk = !clk;
  always @(posedge clk) g <= g + 1;

  class Reader;
    task run();
      repeat (3) begin
        @(posedge clk);
        #1 seen.push_back($past(g, 1, , @(posedge clk)));
      end
    endtask
  endclass

  initial begin
    static Reader r = new;
    r.run();
    $finish;
  end

  // The ticks at 5, 15 and 25 sample g as 0, 1 and 2, and each read stands a
  // step after its tick, so the most recent strictly prior tick is that one.
  final begin
    if (seen != ints_t'{0, 1, 2})
      $fatal(1, "the method read %p, expected '{0, 1, 2}", seen);
    $display("All checks passed");
  end
endmodule
