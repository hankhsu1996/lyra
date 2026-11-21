// A function a module, an interface, a package or the compilation-unit scope
// declares without the automatic keyword is static, with all its declared
// items statically allocated (LRM 13.3.2, 6.21), and the variable named like
// the function that holds its result is one of them (LRM 13.4.1). So it is
// one variable every call shares: it holds its type's default until a call
// writes it, a call reads what the previous call left in it, and a call that
// writes nothing returns that value (LRM 13.4). A function declared static in
// an automatic package is the same. A function declared automatic, and a class
// method, which is automatic (LRM 8.6), make the variable anew on every call.
package pk;
  function int count();
    count = count + 1;
  endfunction

  function int keep(bit write, int v);
    if (write) keep = v;
  endfunction
endpackage

package automatic pa;
  function static int count();
    count = count + 1;
  endfunction
endpackage

function int unit_count();
  unit_count = unit_count + 1;
endfunction

interface Ifc;
  function int count();
    count = count + 1;
  endfunction
endinterface

module Top;
  Ifc ifc ();

  function int count();
    count = count + 1;
  endfunction

  function automatic int fresh();
    fresh = fresh + 1;
  endfunction

  class C;
    function int fresh();
      fresh = fresh + 1;
    endfunction
  endclass

  int module_count;
  int interface_count;
  int package_count;
  int static_in_automatic_count;
  int unit_count_seen;
  int kept;
  int automatic_count;
  int method_count;

  initial begin
    C c;
    c = new;
    repeat (3) begin
      module_count = count();
      interface_count = ifc.count();
      package_count = pk::count();
      static_in_automatic_count = pa::count();
      unit_count_seen = unit_count();
      automatic_count = fresh();
      method_count = c.fresh();
    end
    void'(pk::keep(1, 5));
    kept = pk::keep(0, 9);
  end

  final begin
    if (module_count !== 3)
      $fatal(1, "a module's function counted %0d calls, expected 3", module_count);
    if (interface_count !== 3)
      $fatal(1, "an interface's function counted %0d calls, expected 3", interface_count);
    if (package_count !== 3)
      $fatal(1, "a package's function counted %0d calls, expected 3", package_count);
    if (static_in_automatic_count !== 3)
      $fatal(1, "a static function of an automatic package counted %0d calls, expected 3",
             static_in_automatic_count);
    if (unit_count_seen !== 3)
      $fatal(1, "a compilation-unit function counted %0d calls, expected 3", unit_count_seen);
    if (kept !== 5)
      $fatal(1, "a call writing nothing returned %0d, expected the 5 the call before left",
             kept);
    if (automatic_count !== 1)
      $fatal(1, "an automatic function counted %0d, expected 1 on every call", automatic_count);
    if (method_count !== 1)
      $fatal(1, "a class method counted %0d, expected 1 on every call", method_count);
    $display("All checks passed");
  end
endmodule
