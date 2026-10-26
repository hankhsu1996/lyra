// @remakes: p
//
// A package's class gains a property after the one it had, on a line it
// already has. A module creates an object of the class and calls a method of
// it, which reads nothing of where the class's properties lie or how large its
// objects are, since the class's own unit creates them. So the module means
// what it did.

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
