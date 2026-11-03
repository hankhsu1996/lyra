// @remakes: p
//
// A package's class keeps its method's prototype and gives it another body, on
// the line it was written on: a named block holding an immediate assertion,
// which the class keeps state for between calls. A module creates an object of
// the class and calls the method, which is compiled against what the class
// declares, so the module means what it did.

package p;
  class B;
    int x = 1;
    function int get(); return x; endfunction
  endclass
endpackage

module Top;
  initial begin
    p::B b;
    b = new;
    $display("%0d", b.get());
  end
endmodule
