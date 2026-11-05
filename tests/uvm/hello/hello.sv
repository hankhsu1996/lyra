// The smallest UVM test there is: a test component the factory makes by name,
// which raises an objection over its run phase and reports once inside it. Its
// run is judged as UVM's own examples are, by the report server's summary.

`include "uvm_macros.svh"

module Test;
  import uvm_pkg::*;

  class hello_test extends uvm_test;
    `uvm_component_utils(hello_test)

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      #10 `uvm_info("HELLO", "hello from run_phase", UVM_LOW)
      phase.drop_objection(this);
    endtask
  endclass

  initial run_test("hello_test");
endmodule
