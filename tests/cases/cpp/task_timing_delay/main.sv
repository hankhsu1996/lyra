module Top;
  int q;
  int mid;

  // A delay inside a task suspends the enabling process until the task
  // completes (LRM 13.3): q is not written until t=5.
  task automatic drive(input int v);
    #5;
    q = v;
  endtask

  initial begin
    q = 0;
    drive(7);
  end

  // Samples q at t=2 while drive is still suspended. If the task enable did not
  // span time, q would already be 7 here.
  initial begin
    #2;
    mid = q;
  end
endmodule
