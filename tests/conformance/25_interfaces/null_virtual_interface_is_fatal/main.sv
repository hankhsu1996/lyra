// @error: null virtual interface
//
// A virtual interface holds null until it is assigned, and reaching a component
// through one that holds null is a fatal run-time error (LRM 25.9), unlike a
// null class handle, whose member access the standard leaves indeterminate
// (LRM 8.4).
interface Bus;
  logic [7:0] data;
endinterface

module Top;
  virtual Bus held;

  initial held.data = 8'h01;
endmodule
