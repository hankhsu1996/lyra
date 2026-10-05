// A hierarchical name may leave the instance it is written in and reach
// another instance of the same module (LRM 23.6), and what it reaches there is
// that instance's own: a variable, a subroutine called on it, and a variable
// of a block a loop generate built in it (LRM 27.4). Each of two instances
// reads the other, so each answer is of the instance the path names, never of
// the one the name is written in.
module Peer #(parameter int BASE = 0);
  int value = BASE;
  int other_value = -1;
  int other_called = -1;
  int other_block = -1;

  function automatic int doubled();
    return value * 2;
  endfunction

  for (genvar i = 0; i < 3; i++) begin : lane
    int held = BASE + i;
  end

  initial begin
    #1;
    if (BASE == 10) begin
      other_value = Top.right.value;
      other_called = Top.right.doubled();
      other_block = Top.right.lane[2].held;
    end else begin
      other_value = Top.left.value;
      other_called = Top.left.doubled();
      other_block = Top.left.lane[1].held;
    end
  end
endmodule

module Top;
  Peer #(.BASE(10)) left ();
  Peer #(.BASE(20)) right ();

  final begin
    if (left.other_value !== 20)
      $fatal(1, "left read right.value as %0d", left.other_value);
    if (left.other_called !== 40)
      $fatal(1, "left called right.doubled() and got %0d", left.other_called);
    if (left.other_block !== 22)
      $fatal(1, "left read right.lane[2].held as %0d", left.other_block);
    if (right.other_value !== 10)
      $fatal(1, "right read left.value as %0d", right.other_value);
    if (right.other_called !== 20)
      $fatal(1, "right called left.doubled() and got %0d", right.other_called);
    if (right.other_block !== 11)
      $fatal(1, "right read left.lane[1].held as %0d", right.other_block);
    $display("All checks passed");
  end
endmodule
