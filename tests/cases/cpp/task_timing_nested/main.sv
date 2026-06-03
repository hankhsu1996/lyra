module Top;
  int q;
  int mid;

  task automatic inner();
    #5;
    q = q + 1;
  endtask

  // A task enabling a task that consumes time: control returns to `outer` only
  // after `inner` completes at t=5, so the +10 runs after the +1.
  task automatic outer();
    inner();
    q = q + 10;
  endtask

  initial begin
    q = 0;
    outer();
  end

  initial begin
    #2;
    mid = q;
  end
endmodule
