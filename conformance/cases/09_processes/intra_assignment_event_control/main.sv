// An intra-assignment event control delays the assignment until the event
// occurs, with the right-hand expression evaluated where the statement is
// reached (LRM 9.4.5). A repeat event control waits for that many occurrences of
// the event and reads its count once. A count that is less than or equal to
// zero, unknown, or high impedance at that moment leaves no occurrence to wait
// for at all: of a count of -3 LRM 9.4.5 says the event control "will not
// execute", so the assignment is made where the statement is reached.
module Top;
  logic clk = 0;
  int source = 1;
  int count = 3;
  int negative = -3;
  logic [3:0] unknown = 4'bx;

  int on_edge = 9;
  int on_edge_time = 9;
  int after_three = 9;
  int after_three_time = 9;
  int negative_count = 9;
  int negative_count_time = 9;
  int unknown_count = 9;
  int unknown_count_time = 9;

  // Rising edges at 5, 15, 25, 35 and 45.
  initial repeat (10) #5 clk = ~clk;

  // Rewriting the operand after time zero reaches none of the four assignments.
  initial #1 source = 99;

  initial begin
    on_edge      = @(posedge clk) source;
    on_edge_time = $time;
  end

  initial begin
    after_three      = repeat (count) @(posedge clk) source;
    after_three_time = $time;
  end

  initial begin
    negative_count      = repeat (negative) @(posedge clk) source;
    negative_count_time = $time;
  end

  initial begin
    unknown_count      = repeat (unknown) @(posedge clk) source;
    unknown_count_time = $time;
  end

  final begin
    if (on_edge !== 1 || on_edge_time !== 5)
      $fatal(
          1, "the edge-controlled assignment stored %0d at time %0d, expected 1 at 5", on_edge,
          on_edge_time);
    if (after_three !== 1 || after_three_time !== 25)
      $fatal(
          1, "three occurrences of the edge stored %0d at time %0d, expected 1 at 25", after_three,
          after_three_time);
    if (negative_count !== 1 || negative_count_time !== 0)
      $fatal(
          1, "a negative repeat count stored %0d at time %0d, expected 1 at 0", negative_count,
          negative_count_time);
    if (unknown_count !== 1 || unknown_count_time !== 0)
      $fatal(
          1, "an unknown repeat count stored %0d at time %0d, expected 1 at 0", unknown_count,
          unknown_count_time);
    $display("All checks passed");
  end
endmodule
