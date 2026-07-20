// A package variable (LRM 26.2): static storage the package namespace owns, one
// program-global cell shared across the design, read and written from another
// unit by name. It is initialized once at time zero (LRM 10.5) before any top
// module initializes, so a cross-unit read sees the initialized value. The same
// by-name cell serves a read, a cross-unit write, a package function reading it,
// a package function writing it, and a process waking on its change. A package
// subroutine reaches the runtime services -- needed to wake subscribers on a
// write -- through its own leading services parameter, since a receiver-less
// callable (LRM 26.3) has no `self`.
package pkg;
  int cnt = 5;

  function automatic int doubled();
    return cnt * 2;
  endfunction

  // A package function writes the package variable.
  function automatic void bump();
    cnt = cnt + 1;
  endfunction

  // A write driven by an argument.
  function automatic void add(int n);
    cnt = cnt + n;
  endfunction

  // A package function that writes by calling a sibling package function; the
  // caller forwards its own services parameter to the sibling.
  function automatic void bump_twice();
    bump();
    bump();
  endfunction
endpackage

module Top;
  // Cross-unit read of the initialized value.
  int init_read;
  // Intra-package read: a package function reads the package variable.
  int fn_read;
  // Cross-unit write, then read back.
  int after_write;
  // A write performed inside a package function.
  int after_bump;
  // A write with an argument, inside a package function.
  int after_add;
  // A write performed through a sibling package-function call.
  int after_sibling;
  // Observation: this process wakes when the package variable changes.
  int mirror = 0;

  always @(pkg::cnt) mirror = pkg::cnt;

  initial begin
    init_read = pkg::cnt;       // 5
    fn_read = pkg::doubled();   // 10
    pkg::cnt = 7;               // cross-unit write
    after_write = pkg::cnt;     // 7
    pkg::bump();                // cnt = 8
    after_bump = pkg::cnt;      // 8
    pkg::add(4);                // cnt = 12
    after_add = pkg::cnt;       // 12
    pkg::bump_twice();          // cnt = 14
    after_sibling = pkg::cnt;   // 14
    #1;
    // mirror woke on the final change to 14.
  end
endmodule
