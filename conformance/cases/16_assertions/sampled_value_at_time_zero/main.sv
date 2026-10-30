// At time zero a sampled value is the variable's *default sampled value*, which
// for a static variable is the value its declaration assigns (LRM 16.5.1) --
// not whatever time zero has already written over it.
//
// One procedure writes `d` at time zero and another reads its sampled value
// there. The Active region may run them in either order (LRM 4.4.2.2), so the
// read discriminates: it answers with the declared `8'ha5` whichever ran first,
// where a plain read would answer with one or the other.
module Top;
  logic [7:0] d = 8'hA5;
  int sampled_at_zero;
  int completed;

  initial d = 8'h3C;

  initial begin
    sampled_at_zero = $sampled(d);
    completed = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "the reading procedure did not complete");
    if (sampled_at_zero !== 8'hA5)
      $fatal(
          1, "the sampled value at time zero was %0h, expected a5",
          sampled_at_zero);
    $display("All checks passed");
  end
endmodule
