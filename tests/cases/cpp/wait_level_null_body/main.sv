module Top;
  bit ready;
  int after_wait;

  initial begin
    ready = 0;
    after_wait = 0;
    // Source `wait (cond);` -- statement_or_null is the empty form. The body
    // lowers to a hir::EmptyStmt; the wait still suspends until `ready`
    // becomes truthy, then control falls through to the next statement.
    wait (ready);
    after_wait = 1;
  end

  initial begin
    #5;
    ready = 1;
  end
endmodule
