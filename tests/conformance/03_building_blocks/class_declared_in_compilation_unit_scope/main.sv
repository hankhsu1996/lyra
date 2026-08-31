// A declaration written outside every design element belongs to the
// compilation-unit scope (LRM 3.12.1), which is a name space of its own that
// every design element in the same compilation unit sees. A class declared
// there is reached without qualification, constructs and is assigned like any
// other class handle (LRM 8.3), and its methods run against the object the
// handle refers to.
class Counter;
  int value;

  function void bump(int by);
    value += by;
  endfunction

  function int doubled();
    return value * 2;
  endfunction
endclass

module Top;
  Counter counter;
  int seen = -1;
  int handle_shares_object = -1;

  initial begin
    Counter alias_handle;
    counter = new();
    counter.value = 5;
    counter.bump(2);
    seen = counter.doubled();
    // A class variable holds a handle, so assigning it makes both names refer
    // to the one object (LRM 8.3).
    alias_handle = counter;
    alias_handle.bump(3);
    handle_shares_object = counter.value;
  end

  final begin
    if (seen !== 14) $fatal(1, "seen was %0d, expected 14", seen);
    if (handle_shares_object !== 10)
      $fatal(
          1, "handle_shares_object was %0d, expected 10", handle_shares_object);
    $display("All checks passed");
  end
endmodule
