// LRM 18.16 adds randcase weights as unsigned values at the widest weight's
// width, which wraps, and takes no item where every weight is zero. It does not
// say what a sum of nonzero weights that wraps to zero selects, since no item
// then has a probability of its weight over the sum. Lyra takes no item, as it
// does where every weight is zero.
module Top;
  bit [7:0] half = 128;
  int taken[2];
  int after = 0;

  initial begin
    repeat (100) begin
      randcase
        half: taken[0]++;
        half: taken[1]++;
      endcase
      after++;
    end
  end

  final begin
    if (after != 100)
      $fatal(1, "the loop around the randcase ran %0d times of 100", after);
    if (taken[0] != 0 || taken[1] != 0)
      $fatal(1, "weights adding to zero took %0d and %0d", taken[0], taken[1]);
    $display("All checks passed");
  end
endmodule
