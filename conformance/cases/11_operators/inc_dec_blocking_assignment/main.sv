// The increment and decrement operators behave as blocking assignments: the
// operand takes its new value where the operator is written, and that change
// is a value change other processes can wait on. Inside a nonblocking
// assignment statement the operator still takes effect immediately, while
// the nonblocking update itself is applied at the end of the time step, and
// an index expression on the left-hand side is evaluated at the same time as
// the right-hand side (LRM 11.4.2, 10.4.2).
module Top;
  int counter;
  int trace;
  int nba_source;
  int source_after_statement;
  int nba_target;
  int target_after_statement;
  bit [7:0] nba_vector;
  int nba_index;
  int index_after_statement;

  always @(counter) trace = trace * 10 + counter;

  initial begin
    counter = 0;
    trace = 0;
    target_after_statement = -1;

    nba_source = 5;
    nba_target = 0;
    nba_vector = 8'b0000_0000;
    nba_index = 3;

    nba_target <= nba_source++;
    source_after_statement = nba_source;
    target_after_statement = nba_target;

    nba_vector[nba_index++] <= 1'b1;
    index_after_statement = nba_index;

    #1;
    counter++;
    #1;
    ++counter;
    #1;
    counter--;
  end

  final begin
    if (source_after_statement !== 6)
      $fatal(1, "a++ in a nonblocking source left a at %0d, expected 6",
             source_after_statement);
    if (target_after_statement !== 0)
      $fatal(1, "the nonblocking target was %0d at the statement, expected 0",
             target_after_statement);
    if (nba_target !== 5)
      $fatal(1, "the nonblocking target ended at %0d, expected 5", nba_target);
    if (index_after_statement !== 4)
      $fatal(1, "i++ in a left-hand index left i at %0d, expected 4",
             index_after_statement);
    if (nba_vector !== 8'b0000_1000)
      $fatal(1, "the nonblocking write landed on %b, expected 00001000",
             nba_vector);
    if (counter !== 1)
      $fatal(1, "counter ended at %0d, expected 1", counter);
    if (trace !== 121)
      $fatal(1, "the observer recorded %0d, expected 121", trace);
    $display("All checks passed");
  end
endmodule
