module Top;
  // LRM 21.3.3: $sformatf is the rvalue variant -- the formatted string is
  // the call's return value. This case proves the call composes across
  // expression positions (assign-RHS, function-call argument, condition
  // boolean) and tolerates the function-as-statement discard form.
  int    x;
  string s_assign;
  string s_passed;
  int    branch_taken;
  initial begin
    x = 42;
    // Assign-RHS form.
    s_assign = $sformatf("x=%0d", x);

    // String-comparison-as-condition: proves $sformatf's result behaves
    // like any other string-typed rvalue (no special-casing).
    if ($sformatf("%0d", x) == "42") begin
      branch_taken = 1;
    end else begin
      branch_taken = 0;
    end

    // Bare-call discard form: legal per SV (function-as-statement). The
    // assignment to `s_passed` afterwards proves the discard does not
    // disturb subsequent state.
    $sformatf("ignored=%0d", x);
    s_passed = $sformatf("[%0h]", x);
  end
endmodule
