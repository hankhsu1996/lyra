// SV class inheritance (LRM 8.13): a derived class exposes its base's fields
// through the derived handle, and its own fields sit alongside them. Fields
// are read and written through the same handle to prove the inherited access
// path reaches the same storage as a direct base access would.
module Top;
  class Base;
    int a;
  endclass

  class Derived extends Base;
    int b;
  endclass

  int derived_a;
  int derived_b;

  initial begin
    Derived h;
    h = new;
    h.a = 11;
    h.b = 22;
    derived_a = h.a;
    derived_b = h.b;
  end
endmodule
