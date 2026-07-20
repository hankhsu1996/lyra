`timescale 1ns / 1ps
// Two context task imports run concurrently in different scopes, each suspending
// across the foreign boundary. A context import observes the scope of its own
// declaration; because the DPI scope chain lives on the process, the scope each
// import observes survives its suspension unchanged even while the other import
// is pushing its own scope on another process. A shared thread-global chain
// would let the later import's scope leak into the earlier one on resume.
module Top;
  export "DPI-C" task nap_long;
  export "DPI-C" task nap_short;
  task nap_long();
    #10;
  endtask
  task nap_short();
    #5;
  endtask

  if (1) begin : g1
    import "DPI-C" context task observe_long(output int ok);
    initial begin
      int ok;
      observe_long(ok);
      $display("g1=%0d", ok);
    end
  end

  if (1) begin : g2
    import "DPI-C" context task observe_short(output int ok);
    initial begin
      int ok;
      observe_short(ok);
      $display("g2=%0d", ok);
    end
  end
endmodule
