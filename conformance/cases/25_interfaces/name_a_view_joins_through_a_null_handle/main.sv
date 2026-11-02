// @reports: error
//
// Reaching a name a view joins several declarations under, through a virtual
// interface that holds null, is a fatal run-time error (LRM 25.9), as reaching
// any component of the instance is.
interface Bus;
  logic [3:0] hi, lo;
  modport ctrl(output .Pair({hi, lo}));
endinterface

module Top;
  virtual Bus.ctrl held;

  initial begin
    $display("Ready for the report");
    held.Pair = 8'h3c;
    $display("All checks passed");
  end
endmodule
