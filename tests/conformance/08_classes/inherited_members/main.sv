// A subclass inherits every property and method of its base class as if
// they had been declared in the subclass, and declares its own alongside
// them. The inherited members name one storage: a base method called
// through a subclass handle reads what a write through that handle stored,
// and a method the subclass declares reaches an inherited property without
// qualifying it. A subclass object is also a legal object of its base
// class, so its handle may be assigned to a base class variable and the
// inherited members reached through that variable too (LRM 8.13, 8.14).
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

    function int sum();
      return a + b;
    endfunction
  endclass

  int direct_a;
  int method_a;
  int direct_b;
  int summed;
  int base_view_a;
  int base_view_read;

  initial begin
    Derived d;
    Base b;

    d = new;
    d.set_a(7);
    d.set_b(9);
    direct_a = d.a;
    method_a = d.read_a();
    direct_b = d.b;
    summed = d.sum();

    b = d;
    base_view_a = b.a;
    base_view_read = b.read_a();
  end

  final begin
    if (direct_a !== 7) $fatal(1, "direct_a was %0d, expected 7", direct_a);
    if (method_a !== 7) $fatal(1, "method_a was %0d, expected 7", method_a);
    if (direct_b !== 9) $fatal(1, "direct_b was %0d, expected 9", direct_b);
    if (summed !== 16) $fatal(1, "summed was %0d, expected 16", summed);
    if (base_view_a !== 7)
      $fatal(1, "base_view_a was %0d, expected 7", base_view_a);
    if (base_view_read !== 7)
      $fatal(1, "base_view_read was %0d, expected 7", base_view_read);
    $display("All checks passed");
  end
endmodule
