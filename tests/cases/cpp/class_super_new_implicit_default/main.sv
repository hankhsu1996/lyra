// LRM 8.7 implicit forwarding: a derived class that omits `super.new`
// still runs the base's constructor as its first act. The base's own
// initializer sets its property; the derived's body then sets its own.
// Both values reach the observed variables because the implicit base call
// still fires -- the base_val would be zero if the base ctor were skipped.
module Top;
  class Base;
    int b_val;
    function new();
      b_val = 42;
    endfunction
  endclass

  class Derived extends Base;
    int d_val;
    function new();
      d_val = 99;
    endfunction
  endclass

  int base_val;
  int derived_val;

  initial begin
    Derived d;
    d = new();
    base_val = d.b_val;
    derived_val = d.d_val;
  end
endmodule
