// A write to a member of a tagged union checks the union's tag before it
// writes (LRM 11.9), and that check is part of evaluating the write. An operand
// of ?: that is not needed is not evaluated, and neither its side effects nor
// its run-time errors occur (LRM 11.3.5). So a write in the arm ?: does not
// take neither runs the function in its index nor fails its check, though the
// union holds another member than the one the arm would write.
module Top;
  typedef union tagged packed {
    logic [3:0] a;
    logic [3:0] b;
  } choice_t;

  choice_t choices [2];
  int calls;
  bit low;
  int written;
  int index_ran = -1;

  function automatic int counted_index();
    calls = calls + 1;
    return 1;
  endfunction

  initial begin
    low = 1'b0;
    choices[1] = tagged b 4'h3;
    calls = 0;
    written = low ? int'(choices[counted_index()].a = 4'h4) : 3;
    index_ran = calls;
  end

  final begin
    if (index_ran !== 0)
      $fatal(1, "the arm ?: did not take ran its index %0d times, expected 0",
             index_ran);
    if (written !== 3)
      $fatal(1, "?: chose %0d, expected 3", written);
    if (choices[1] !== choice_t'(tagged b 4'h3))
      $fatal(1, "the arm ?: did not take changed the union");
    $display("All checks passed");
  end
endmodule
