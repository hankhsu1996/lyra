// @remakes: pkg
//
// A package gains a class and a function, on a line it already has. A module
// creates an object of the other class the package declares, reads its
// property and calls its method, none of which is what was added, so the
// module means what it did.

package pkg; class Spare; int s; endclass function automatic int spare(); return 2; endfunction
  class Counter;
    int n = 1;
    function int next(); n++; return n; endfunction
  endclass
endpackage

module Top;
  initial begin
    pkg::Counter c;
    c = new();
    $display("%0d %0d", c.n, c.next());
  end
endmodule
