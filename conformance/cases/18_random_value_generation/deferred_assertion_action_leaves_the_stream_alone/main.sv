// Random stability is the property that a thread's or an initializer's
// sequence of random values depends only on its own seed and on the order of
// its own draws and thread creations (LRM 18.14, 18.14.1). Reaching a deferred
// assertion draws nothing: its action is a subroutine call placed in a queue
// and executed in the Reactive region (LRM 16.4.1). So whether the assertion
// fails does not move the stream of the static initializer or the procedure
// that reached it, in what they draw before the action runs. Which stream the
// action itself draws from the standard does not say, so nothing here is drawn
// after it.
//
// Two instances of one module start from the same seed (LRM 18.14.1) and
// differ only in whether their assertion fails, so what each draws outside the
// action matches the other's.
module FromInitializer #(
    parameter bit Holds = 1
);
  int unsigned action_drew;

  function automatic void draw_in_action();
    action_drew = $urandom;
  endfunction

  function automatic int unsigned initialize();
    int unsigned first;
    assert #0 (Holds) else draw_in_action();
    first = $urandom;
    return first ^ $urandom;
  endfunction

  int unsigned drew = initialize();
endmodule

module FromProcedure #(
    parameter bit Holds = 1
);
  int unsigned action_drew;
  int unsigned drew;

  function automatic void draw_in_action();
    action_drew = $urandom;
  endfunction

  initial begin
    int unsigned first;
    assert #0 (Holds) else draw_in_action();
    first = $urandom;
    drew = first ^ $urandom;
  end
endmodule

module Top;
  FromInitializer #(.Holds(1)) init_holds ();
  FromInitializer #(.Holds(0)) init_fails ();
  FromProcedure #(.Holds(1)) proc_holds ();
  FromProcedure #(.Holds(0)) proc_fails ();

  final begin
    if (init_fails.action_drew === 0 || proc_fails.action_drew === 0)
      $fatal(1, "a failing assertion's action did not draw");
    if (init_holds.drew !== init_fails.drew)
      $fatal(1, "the initializers drew %0h and %0h, expected the same value",
             init_holds.drew, init_fails.drew);
    if (proc_holds.drew !== proc_fails.drew)
      $fatal(1, "the procedures drew %0h and %0h, expected the same value",
             proc_holds.drew, proc_fails.drew);
    $display("All checks passed");
  end
endmodule
