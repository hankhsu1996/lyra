// SV class inheritance (LRM 8.13, LRM 8.14): a base method is callable through
// a derived handle without an explicit qualifier, and the call reaches the
// base's implementation because the derived class does not override it. The
// method writes and reads base and derived fields to prove `self` reaches
// the same object.
module Top;
  class Base;
    int a;
    function void set_a(int v);
      a = v;
    endfunction
    function int read_a();
      return a;
    endfunction
  endclass

  class Derived extends Base;
    int b;
    function void set_b(int v);
      b = v;
    endfunction
  endclass

  int direct_a;
  int method_a;
  int direct_b;

  initial begin
    Derived h;
    h = new;
    h.set_a(7);
    h.set_b(9);
    direct_a = h.a;
    method_a = h.read_a();
    direct_b = h.b;
  end
endmodule
