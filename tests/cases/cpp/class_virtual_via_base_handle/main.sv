// LRM 8.20 virtual dispatch through a base-typed handle: the static type of
// the handle is Base but its dynamic type is Derived; a virtual call
// resolves by the dynamic type, so the Derived implementation runs. This is
// the shape virtual dispatch is defined to support -- the receiver's static
// type would resolve the wrong method under any static-scoped rule.
module Top;
  class Base;
    virtual function int tag();
      return 100;
    endfunction
  endclass

  class Derived extends Base;
    function int tag();
      return 200;
    endfunction
  endclass

  int through_base_handle;
  int through_derived_handle;

  initial begin
    Base h;
    Derived d;
    d = new;
    h = d;
    through_base_handle = h.tag();
    through_derived_handle = d.tag();
  end
endmodule
