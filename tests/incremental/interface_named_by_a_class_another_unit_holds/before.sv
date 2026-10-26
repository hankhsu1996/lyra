// @remakes: p
//
// A package's class comes to implement a second interface class, adding the
// method it requires after the ones it had, on lines it already has. A module
// only holds a handle to the class, which reads nothing of what the class
// declares; a second creates an object of it and calls through the first
// interface class, whose table the class still gives the same. So neither
// means anything else.

package i;
  interface class I;
    pure virtual function int f();
  endclass
  interface class J;
    pure virtual function int g();
  endclass
endpackage

package p;
  class B implements i::I;
    virtual function int f(); return 1; endfunction
  endclass
endpackage

module Holds;
  p::B kept;
  initial kept = null;
endmodule

module Top;
  Holds h ();
  initial begin
    p::B b;
    i::I through;
    b = new;
    through = b;
    $display("%0d", through.f());
  end
endmodule
