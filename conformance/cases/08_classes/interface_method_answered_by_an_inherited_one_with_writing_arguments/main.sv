// A class may meet an interface class's method with one it inherits from the
// class it extends (LRM 8.26.2), and a call through a handle of the interface
// class then runs that inherited method, whose `output`, `inout` and `ref`
// formals write back to the caller's actuals as a direct call's do (LRM
// 13.5.1, 13.5.2). A task among them writes its `output` back when it
// completes (LRM 13.3).
interface class Exchange;
  pure virtual function int swap(output int out, inout int both, ref int shared);
  pure virtual task fill(output int out, input int value);
endclass

class Worker;
  virtual function int swap(output int out, inout int both, ref int shared);
    out = both;
    both = shared;
    shared = 40;
    return 7;
  endfunction

  virtual task fill(output int out, input int value);
    #1 out = value + 1;
  endtask
endclass

class Inheriting extends Worker implements Exchange;
endclass

module Top;
  int out_actual;
  int both_actual;
  int shared_actual;
  int returned;
  int filled;
  time filled_at;

  initial begin
    Inheriting object;
    Exchange through;
    object = new;
    through = object;
    out_actual = -1;
    both_actual = 11;
    shared_actual = 22;
    returned = -1;
    filled = -1;
    filled_at = 0;
    returned = through.swap(out_actual, both_actual, shared_actual);
    through.fill(filled, 99);
    filled_at = $time;
  end

  final begin
    if (returned !== 7) $fatal(1, "returned was %0d, expected 7", returned);
    if (out_actual !== 11)
      $fatal(1, "out_actual was %0d, expected 11", out_actual);
    if (both_actual !== 22)
      $fatal(1, "both_actual was %0d, expected 22", both_actual);
    if (shared_actual !== 40)
      $fatal(1, "shared_actual was %0d, expected 40", shared_actual);
    if (filled !== 100) $fatal(1, "filled was %0d, expected 100", filled);
    if (filled_at !== 1) $fatal(1, "filled_at was %0t, expected 1", filled_at);
    $display("All checks passed");
  end
endmodule
