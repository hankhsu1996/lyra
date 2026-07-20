// LRM 35.5.2 time-consuming DPI-C task: a foreign C task, reached from a running
// simulation, calls back an exported SV task that consumes simulation time. The
// exported body suspends on a delay across the foreign boundary and resumes when
// the scheduler advances time -- the foreign call stack is parked on a fiber
// while simulation time moves, then continues. Two delays in one exported body
// exercise repeated suspend/resume across the same call.
module Top;
  import "DPI-C" task run_it();

  export "DPI-C" task step;

  int count;

  task step();
    #5;
    count = count + 1;
    #3;
    count = count + 1;
  endtask

  initial begin
    count = 0;
    run_it();
    $display("count=%0d time=%0d", count, $time);
  end
endmodule
