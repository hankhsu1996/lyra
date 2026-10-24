// @remakes: q Top
//
// A class between the one a module's class extends and the one declaring
// post_randomize() gains a post_randomize() of its own, on a line it already
// has. A call naming the hook through super runs the hook of the nearest class
// above that declares one (LRM 18.6.2), so the module's call means another
// method although its own text did not change, and the package holding the
// class between changed; the package declaring the first hook did not.

package p;
  class B;
    int log = 0;
    function void post_randomize(); log = log * 10 + 1; endfunction
  endclass
endpackage

package q;
  class M extends p::B;
    function void post_randomize(); log = log * 10 + 9; endfunction
  endclass
endpackage

module Top;
  class S extends q::M;
    function void post_randomize(); super.post_randomize(); log = log * 10 + 2; endfunction
  endclass
  initial begin
    S s = new;
    s.post_randomize();
    $display("%0d", s.log);
  end
endmodule
