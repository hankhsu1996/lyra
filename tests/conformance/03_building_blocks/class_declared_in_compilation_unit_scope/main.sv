// A declaration written outside every design element belongs to the
// compilation-unit scope (LRM 3.12.1), which is a name space of its own that
// every design element in the same compilation unit sees. A class declared
// there is reached without qualification, constructs and is assigned like any
// other class handle (LRM 8.3), and its methods run against the object the
// handle refers to. A method is a body of that scope like any other, so it
// calls a subroutine the compilation unit declares by its simple name.
function automatic int tripled(int v);
  return v * 3;
endfunction

class Counter;
  int value;

  function void bump(int by);
    value += by;
  endfunction

  function int doubled();
    return value * 2;
  endfunction

  function int scaled();
    return tripled(value);
  endfunction
endclass

module Top;
  Counter counter;
  int seen = -1;
  int through_unit_subroutine = -1;
  int handle_shares_object = -1;

  initial begin
    Counter alias_handle;
    counter = new();
    counter.value = 5;
    counter.bump(2);
    seen = counter.doubled();
    through_unit_subroutine = counter.scaled();
    // A class variable holds a handle, so assigning it makes both names refer
    // to the one object (LRM 8.3).
    alias_handle = counter;
    alias_handle.bump(3);
    handle_shares_object = counter.value;
  end

  final begin
    if (seen !== 14) $fatal(1, "seen was %0d, expected 14", seen);
    if (through_unit_subroutine !== 21)
      $fatal(
          1, "through_unit_subroutine was %0d, expected 21",
          through_unit_subroutine);
    if (handle_shares_object !== 10)
      $fatal(
          1, "handle_shares_object was %0d, expected 10", handle_shares_object);
    $display("All checks passed");
  end
endmodule
