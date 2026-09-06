// An event control's expression may read more than one variable, and what is
// watched is still the value of the expression rather than the variables it is
// built from (LRM 9.4.2): a change in any operand without a change in the
// result is not an event, and a change in the result is one whichever operand
// carried it. The expression need only reduce to a singular value, so a
// concatenation, an operator over several variables, and a select whose index
// is itself read all name one.
module Top;
  logic a = 1;
  logic b = 0;
  logic [1:0] pair = 2'b00;
  int mem[0:3];
  int idx = 0;

  time concatenation_at;
  time reduction_at;
  int reduction_wakes;
  time dynamic_index_at;

  initial begin
    @({pair[1], pair[0]});
    concatenation_at = $time;
  end

  initial begin
    forever begin
      @(a | b);
      reduction_wakes = reduction_wakes + 1;
      reduction_at = $time;
    end
  end

  initial begin
    @(mem[idx]);
    dynamic_index_at = $time;
  end

  initial begin
    // `a | b` is 1 before and after, so raising b is a change in an operand
    // and no event. Writing an element the select does not name is likewise a
    // change in `mem` and none in `mem[idx]`.
    #5;
    b = 1;
    mem[1] = 42;
    #5;
    a = 0;
    b = 0;
    pair[1] = 1'b1;
    mem[0] = 7;
  end

  final begin
    if (concatenation_at !== 10)
      $fatal(1, "concatenation_at was %0d, expected 10", concatenation_at);
    if (reduction_wakes !== 1)
      $fatal(1, "reduction_wakes was %0d, expected 1", reduction_wakes);
    if (reduction_at !== 10)
      $fatal(1, "reduction_at was %0d, expected 10", reduction_at);
    if (dynamic_index_at !== 10)
      $fatal(1, "dynamic_index_at was %0d, expected 10", dynamic_index_at);
    $display("All checks passed");
  end
endmodule
