// A package function called from a separate module (LRM 26.3). The package is a
// namespace: its function is a receiver-less static callable owned by the unit,
// not an instance method of any object. The call crosses the compilation-unit
// boundary and reaches the function by name.
package pkg;
  localparam int Base = 100;
  function automatic int add_base(int x);
    return x + Base;
  endfunction
endpackage

module Top;
  int r;
  initial begin
    r = pkg::add_base(23);
  end
endmodule
