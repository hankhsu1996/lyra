// A virtual method's override need not give a formal the same default as the
// method it overrides, only a default where it has one (LRM 8.20), and which
// of the two a call through a base class handle takes the standard does not
// say. Lyra takes the default of the method the call names, that of the
// handle's class, as C++ does, and runs the override's body with it. This case
// holds Lyra to that.
module Top;
  class Base;
    int p = 5;
    virtual function int f(int x = p);
      return x;
    endfunction
  endclass

  class Derived extends Base;
    virtual function int f(int x = p + 1000);
      return x + 1;
    endfunction
  endclass

  initial begin
    Base h;
    Derived d;
    d = new;
    d.p = 20;
    h = d;
    if (h.f() !== 21)
      $fatal(1, "an override through a base handle gave %0d, expected 21",
             h.f());
    $display("All checks passed");
  end
endmodule
