// A class declared inside a module is a type of each instance of the module,
// unique to that instance (LRM 6.22), so a dynamic cast toward it succeeds only
// for an object of the class that belongs to the instance the cast names (LRM
// 8.16). A class extending one the instance declares is of that instance's base,
// whichever scope of the instance declares it.
class Base;
  virtual function int id();
    return 0;
  endfunction
endclass

module M;
  class C extends Base;
    virtual function int id();
      return 1;
    endfunction
  endclass
  class D extends C;
    virtual function int id();
      return 2;
    endfunction
  endclass
  for (genvar i = 0; i < 1; i++) begin : g
    class E extends C;
      virtual function int id();
        return 3;
      endfunction
    endclass
    E e;
    initial e = new;
  end
  C c;
  D d;
  initial begin
    c = new;
    d = new;
  end

  function automatic int is_c(Base b);
    C as_c;
    return $cast(as_c, b);
  endfunction
  function automatic int is_d(Base b);
    D as_d;
    return $cast(as_d, b);
  endfunction
endmodule

module Top;
  M m1();
  M m2();

  initial begin
    #1;
    if (m1.is_c(m1.c) !== 1) $fatal(1, "an object of its own instance's class");
    if (m1.is_c(m2.c) !== 0) $fatal(1, "an object of another instance's class");
    if (m2.is_c(m1.c) !== 0) $fatal(1, "an object of another instance's class");
    if (m1.is_c(m1.d) !== 1) $fatal(1, "an object of its own derived class");
    if (m1.is_c(m2.d) !== 0)
      $fatal(1, "an object of another instance's derived class");
    if (m1.is_c(m1.g[0].e) !== 1)
      $fatal(1, "an object of a class its generate block derives");
    if (m2.is_c(m1.g[0].e) !== 0)
      $fatal(1, "another instance's generate block's derived class");
    if (m1.is_d(m1.c) !== 0) $fatal(1, "a base object is not of its subclass");
    if (m1.is_d(m1.d) !== 1) $fatal(1, "an object of its own subclass");
    if (m2.is_d(m1.d) !== 0) $fatal(1, "another instance's subclass");
    $display("All checks passed");
  end
endmodule
