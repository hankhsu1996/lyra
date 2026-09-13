// A context import is supplied the instantiated scope of its own declaration,
// and that is a property of the declaration rather than of when the call is
// made (LRM 35.5.3). A variable declaration assignment runs before any
// procedure starts (LRM 10.5, and LRM 26.2 says the same for a package), so
// every position that initializes static-lifetime state -- a module variable, a
// package variable, a static class property, a static local of a subroutine --
// reaches the import with no process executing and must still observe the same
// scope a call made from a procedure does.
package P;
  import "DPI-C" context function string where();

  string from_package_init = where();
  string from_package_task;

  task automatic observe();
    from_package_task = where();
  endtask
endpackage

module Top;
  import "DPI-C" context function string where();

  string from_module_init = where();
  string from_process;

  class C;
    static string from_static_property = where();
  endclass

  function automatic string first_seen();
    static string first = where();
    return first;
  endfunction

  string from_static_local;

  initial begin
    from_process = where();
    // A subroutine's static local is initialized before time zero whether or
    // not the subroutine is ever called (LRM 6.21), so what this reads back is
    // what the initializer observed, not what this call observes.
    from_static_local = first_seen();
    P::observe();
  end

  final begin
    // The scope a procedure observes is the one every other position is held
    // to, so it is checked first and against a value written here by hand.
    if (from_process != "Top")
      $fatal(1, "from a procedure the scope was '%s', expected 'Top'",
             from_process);
    if (from_module_init != "Top")
      $fatal(1, "from a module variable initializer the scope was '%s'",
             from_module_init);
    if (C::from_static_property != "Top")
      $fatal(1, "from a static class property the scope was '%s'",
             C::from_static_property);
    if (from_static_local != "Top")
      $fatal(1, "from a subroutine's static local the scope was '%s'",
             from_static_local);
    // A package is never instantiated, so what its own declaration observes is
    // whatever a call from a package procedure observes -- the point being that
    // the two agree, which is what makes the answer the declaration's.
    if (P::from_package_init != P::from_package_task)
      $fatal(
          1, "in a package the initializer saw '%s' and a task saw '%s'",
          P::from_package_init, P::from_package_task);
    $display("All checks passed");
  end
endmodule
