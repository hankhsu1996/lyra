`timescale 1ns / 1ps
// Two instances of one module each run a context import that suspends across the
// foreign boundary inside the export it calls back. Import and export are
// declared in the same scope, which is what lets the export be called directly
// (LRM 35.5.3), and each instance is its own scope. Because the DPI scope chain
// lives on the process, the scope an import observes survives its suspension
// unchanged even while the other instance is pushing its own scope on another
// process. A shared thread-global chain would let the later import's scope leak
// into the earlier one on resume.
module Sub #(parameter int NAP = 10);
  export "DPI-C" task nap;
  task nap();
    #NAP;
  endtask

  import "DPI-C" context task observe(output int ok);
  initial begin
    int ok;
    observe(ok);
    $display("nap%0d=%0d", NAP, ok);
  end
endmodule

module Top;
  Sub #(.NAP(10)) s_long ();
  Sub #(.NAP(5)) s_short ();
endmodule
