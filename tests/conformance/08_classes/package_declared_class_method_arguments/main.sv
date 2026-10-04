// A method of a class is a subroutine, so a call to one passes its input
// values in and passes the values of its output and inout formals back at the
// return, and a ref formal shares the caller's variable for the length of the
// call (LRM 8.6, 13.5, 13.5.2). That holds unchanged when the class is declared
// in a package and the call is written in another scope (LRM 26.3): for a
// method called on an object, for a static method called on the class (LRM
// 8.10), for a method a class of another package inherits (LRM 8.13), for a
// virtual method whose body the object selects (LRM 8.20), and for a task,
// whose output reaches its actual only when the task returns while a ref it
// was handed carries the write out at once.
package pkg;
  class Meter;
    int scale = 3;
    static int calls = 0;

    function void scaled(input int a, output int b);
      b = a * scale;
    endfunction

    function void bump(inout int v);
      v = v + scale;
    endfunction

    function void ref_set(ref int r, input int val);
      r = val + scale;
    endfunction

    function int const_ref_read(const ref int r);
      return r + scale;
    endfunction

    function int div_mod(input int a, input int b, output int rem);
      rem = a % b;
      return a / b;
    endfunction

    static function void count(input int step, output int total);
      calls = calls + step;
      total = calls;
    endfunction

    virtual function void describe(input int a, output int kind,
                                   output int value);
      kind = 1;
      value = a + scale;
    endfunction

    task delayed(input int a, output int b, ref int r);
      b = a + 100;
      r = r + 1;
      #5;
    endtask
  endclass
endpackage

package fine_pkg;
  class FineMeter extends pkg::Meter;
    virtual function void describe(input int a, output int kind,
                                   output int value);
      kind = 2;
      value = a * scale;
    endfunction
  endclass
endpackage

module Top;
  int out_b;
  int inout_v;
  int ref_r;
  int cref_in;
  int cref_out;
  int quotient;
  int rem;
  int total_first;
  int total_second;
  int base_kind;
  int base_value;
  int fine_kind;
  int fine_value;
  int inherited_b;
  int early_b;
  int early_r;
  int mid_b;
  int mid_r;

  initial begin
    pkg::Meter m;
    fine_pkg::FineMeter fine;
    pkg::Meter through_base;

    m = new;
    fine = new;
    through_base = fine;

    m.scaled(7, out_b);
    inout_v = 5;
    m.bump(inout_v);
    m.ref_set(ref_r, 70);
    cref_in = 8;
    cref_out = m.const_ref_read(cref_in);
    quotient = m.div_mod(17, 5, rem);
    pkg::Meter::count(4, total_first);
    pkg::Meter::count(2, total_second);
    m.describe(10, base_kind, base_value);
    through_base.describe(10, fine_kind, fine_value);
    fine.scaled(5, inherited_b);
  end

  initial begin
    pkg::Meter timed;

    timed = new;
    early_b = 3;
    early_r = 200;
    timed.delayed(1, early_b, early_r);
  end

  initial begin
    #2;
    mid_b = early_b;
    mid_r = early_r;
  end

  final begin
    if (out_b !== 21) $fatal(1, "out_b was %0d, expected 21", out_b);
    if (inout_v !== 8) $fatal(1, "inout_v was %0d, expected 8", inout_v);
    if (ref_r !== 73) $fatal(1, "ref_r was %0d, expected 73", ref_r);
    if (cref_in !== 8) $fatal(1, "cref_in was %0d, expected 8", cref_in);
    if (cref_out !== 11) $fatal(1, "cref_out was %0d, expected 11", cref_out);
    if (quotient !== 3) $fatal(1, "quotient was %0d, expected 3", quotient);
    if (rem !== 2) $fatal(1, "rem was %0d, expected 2", rem);
    if (total_first !== 4)
      $fatal(1, "total_first was %0d, expected 4", total_first);
    if (total_second !== 6)
      $fatal(1, "total_second was %0d, expected 6", total_second);
    if (base_kind !== 1) $fatal(1, "base_kind was %0d, expected 1", base_kind);
    if (base_value !== 13)
      $fatal(1, "base_value was %0d, expected 13", base_value);
    // The handle is of the base class and the object is of the extension, so
    // the body that ran is the extension's.
    if (fine_kind !== 2) $fatal(1, "fine_kind was %0d, expected 2", fine_kind);
    if (fine_value !== 30)
      $fatal(1, "fine_value was %0d, expected 30", fine_value);
    if (inherited_b !== 15)
      $fatal(1, "inherited_b was %0d, expected 15", inherited_b);
    if (mid_b !== 3) $fatal(1, "mid_b was %0d, expected 3", mid_b);
    if (mid_r !== 201) $fatal(1, "mid_r was %0d, expected 201", mid_r);
    if (early_b !== 101) $fatal(1, "early_b was %0d, expected 101", early_b);
    if (early_r !== 201) $fatal(1, "early_r was %0d, expected 201", early_r);
    $display("All checks passed");
  end
endmodule
