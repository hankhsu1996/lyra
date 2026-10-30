// The right-hand side of a continuous assignment can be any expression that
// evaluates to a value (LRM 10.2, 10.3.2). A call to a user-defined function, a
// replication, a set-membership test, a conditional whose predicate conjoins
// two conditions and the system functions that only query a value all qualify,
// and each re-evaluates with the rest of the expression when an operand of it
// changes -- including the current simulation time, which the assignment reads
// afresh every time it is driven again (LRM 20.3.2).
module Top;
  function automatic int doubled(int x);
    return x * 2;
  endfunction

  logic [3:0] operand;
  bit selector;
  int trigger;

  int call_result;
  logic [11:0] replicated;
  bit in_set;
  int guarded;
  int stamped;
  bit unmatched_plusarg;
  string formatted;

  assign call_result = doubled(operand);
  assign replicated = {3{operand}};
  assign in_set = operand inside {4'd1, 4'd5, 4'd9};
  assign guarded = selector &&& (operand > 4'd9) ? 1 : 2;
  assign stamped = trigger + $stime;
  assign unmatched_plusarg = $test$plusargs("no_such_switch");
  assign formatted = $sformatf("operand=%0d", operand);

  initial begin
    operand = 4'd5;
    selector = 1'b1;
    #7;
    trigger = 1;
    #1;
  end

  final begin
    if (call_result !== 10)
      $fatal(1, "call_result was %0d, expected 10", call_result);
    if (replicated !== 12'h555)
      $fatal(1, "replicated was %h, expected 555", replicated);
    if (in_set !== 1'b1) $fatal(1, "in_set was %b, expected 1", in_set);
    if (guarded !== 2) $fatal(1, "guarded was %0d, expected 2", guarded);
    if (stamped !== 8) $fatal(1, "stamped was %0d, expected 8", stamped);
    if (unmatched_plusarg !== 1'b0)
      $fatal(1, "unmatched_plusarg was %b, expected 0", unmatched_plusarg);
    if (formatted != "operand=5")
      $fatal(1, "formatted was '%s', expected 'operand=5'", formatted);
    $display("All checks passed");
  end
endmodule
