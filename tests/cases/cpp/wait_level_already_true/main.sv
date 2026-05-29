module Top;
  bit ready;
  int marker;

  initial begin
    ready = 1;
    marker = 0;
    // LRM 9.4.3: if `ready` is already true on entry, the wait does not
    // suspend; the body runs in the same time step.
    wait (ready) marker = 1;
  end
endmodule
