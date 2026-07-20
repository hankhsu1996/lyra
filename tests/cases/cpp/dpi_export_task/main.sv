// LRM 35.8 DPI-C export task: a foreign C function, reached from a running
// simulation through an imported task, calls back exported SV tasks. An
// exported task rides the same task call protocol as any SV task (LRM 13.4.4);
// its foreign wrapper drives the task body to completion and returns the DPI
// disable-acknowledgment int (0 here, no disable active). This exercises the
// full import -> export task chain under Lyra as the driver.
//
// Coverage in one module: an input-only export task that writes module state
// through its receiver, a void export task with two scalar `output` formals,
// and an export task with a scalar `inout`. The imported `drive` calls each and
// folds every result back into `total`, so a marshaling or drive defect in any
// direction moves the asserted total.
module Top;
  import "DPI-C" task drive();

  export "DPI-C" task add_to_total;
  export "DPI-C" task get_pair;
  export "DPI-C" task accumulate;

  int total;

  task add_to_total(input int delta);
    total = total + delta;
  endtask

  task get_pair(output int lo, output int hi);
    lo = 3;
    hi = 7;
  endtask

  task accumulate(inout int acc);
    acc = acc + 100;
  endtask

  initial begin
    total = 0;
    drive();
    $display("total=%0d", total);
  end
endmodule
