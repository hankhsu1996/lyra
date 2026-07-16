// LRM 8.7 explicit `super.new(args)`: the derived's constructor forwards
// arguments to the base's constructor before its own body runs, so a
// property the base sets from its formal is initialized to the value the
// derived passed. The base's property is read through the derived handle to
// confirm the forwarding actually happened.
module Top;
  class Base;
    int b_val;
    function new(int a);
      b_val = a * 3;
    endfunction
  endclass

  class Derived extends Base;
    int d_val;
    function new(int a, int b);
      super.new(a);
      d_val = b;
    endfunction
  endclass

  int base_val;
  int derived_val;

  initial begin
    Derived d;
    d = new(4, 7);
    base_val = d.b_val;
    derived_val = d.d_val;
  end
endmodule
