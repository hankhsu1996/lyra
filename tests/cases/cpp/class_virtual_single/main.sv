// LRM 8.20 virtual method: a `virtual function` in the base declares a
// dispatch slot; a derived class's same-signature function implicitly
// overrides it. Calling through a derived handle reaches the derived
// implementation because the slot resolves by the receiver's dynamic type.
module Top;
  class Base;
    virtual function int classify();
      return 1;
    endfunction
  endclass

  class Derived extends Base;
    function int classify();
      return 2;
    endfunction
  endclass

  int base_result;
  int derived_result;

  initial begin
    Base b;
    Derived d;
    b = new;
    d = new;
    base_result = b.classify();
    derived_result = d.classify();
  end
endmodule
