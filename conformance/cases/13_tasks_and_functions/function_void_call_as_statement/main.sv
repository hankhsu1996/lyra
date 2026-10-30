// A void function has no return value, so a call to it is a statement rather
// than an operand, and what the call leaves behind is whatever its body wrote
// outside itself (LRM 13.4.1).
module Top;
  int counter;

  function automatic void increment();
    counter = counter + 1;
  endfunction

  initial begin
    counter = 0;
    increment();
    increment();
    increment();
  end

  final begin
    if (counter !== 3) $fatal(1, "counter was %0d, expected 3", counter);
    $display("All checks passed");
  end
endmodule
