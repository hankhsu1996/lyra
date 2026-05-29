module Top;
  bit ready;
  int marker;

  initial begin
    ready = 0;
    marker = 0;
    // LRM 9.4.3: cond is false on entry, so the process subscribes to the
    // read set of cond (here just `ready`) and re-evaluates on any change.
    wait (ready) marker = 1;
  end

  initial begin
    #5;
    ready = 1;
  end
endmodule
