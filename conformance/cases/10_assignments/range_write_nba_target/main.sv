// A nonblocking assignment whose left-hand side is a part-select evaluates
// the index expression at the same time as the right-hand side and then
// schedules the update, which is applied at the end of the time step. A read
// taken between the statement and the end of the step still sees the old
// value, and changing the index variable afterwards does not move where the
// update lands (LRM 10.4.2).
module Top;
  bit [31:0] data;
  bit [31:0] at_the_statement;
  bit [31:0] after_the_step;
  int idx;

  initial begin
    at_the_statement = 32'hFFFFFFFF;

    data = 32'h0000ABCD;
    idx = 8;
    data[idx +: 8] <= 8'hEF;
    idx = 24;
    at_the_statement = data;
    #1;
    after_the_step = data;
  end

  final begin
    if (at_the_statement !== 32'h0000ABCD)
      $fatal(1, "the target was %h at the statement, expected 0000abcd",
             at_the_statement);
    if (after_the_step !== 32'h0000EFCD)
      $fatal(1, "the target was %h after the step, expected 0000efcd",
             after_the_step);
    $display("All checks passed");
  end
endmodule
