// A package variable (LRM 26.2): static storage the package namespace owns, one
// program-global cell shared across the design, read and written from another
// unit by name. It is initialized once at time zero (LRM 10.5) before any top
// module initializes, so a cross-unit read sees the initialized value. The same
// by-name cell serves a read, a write, a package function reading it, and a
// process waking on its change.
package pkg;
  int cnt = 5;

  function automatic int doubled();
    return cnt * 2;
  endfunction
endpackage

module Top;
  // Cross-unit read of the initialized value.
  int init_read;
  // Intra-package read: a package function reads the package variable.
  int fn_read;
  // Cross-unit write, then read back.
  int after_write;
  // Observation: this process wakes when the package variable changes.
  int mirror = 0;

  always @(pkg::cnt) mirror = pkg::cnt + 1;

  initial begin
    init_read = pkg::cnt;
    fn_read = pkg::doubled();
    pkg::cnt = 7;
    #1;
    after_write = pkg::cnt;
  end
endmodule
