// Execution of a subroutine call passes the input values in, and execution of
// the return passes the values of the output and inout formals back to the
// corresponding variables in the call (LRM 13.5). A ref formal is not copied at
// all: caller and callee share the one variable, so a write the callee makes is
// visible outside immediately, and a const ref formal may be read but not
// written (LRM 13.5.2). A subroutine a package declares and another scope calls
// by its resolved name is bound by those rules unchanged (LRM 26.3), including
// when it writes a return value and an output together, and when it suspends:
// a task's output reaches its actual only at the return, however long the body
// waits after writing the formal, while a ref the same task was handed carries
// the write out at once.
package pkg;
  function automatic void make_double(input int a, output int b);
    b = a * 2;
  endfunction

  function automatic void bump(inout int v);
    v = v + 10;
  endfunction

  function automatic void ref_set(ref int r, input int val);
    r = val;
  endfunction

  function automatic int const_ref_read(const ref int r);
    return r + 1;
  endfunction

  function automatic int div_mod(input int a, input int b, output int rem);
    rem = a % b;
    return a / b;
  endfunction

  function automatic int scaled(input int a, output int o);
    o = a + 1;
    return a * 3;
  endfunction

  task automatic delayed_out(input int a, output int b);
    #1;
    b = a + 100;
  endtask

  task automatic early_out(input int a, output int b, ref int r);
    b = a + 100;
    r = r + 1;
    #5;
  endtask
endpackage

module Top;
  int out_b;
  int inout_v;
  int ref_r;
  int cref_in;
  int cref_out;
  int quotient;
  int rem;
  int task_out;
  int nested_ret;
  int nested_o;
  int early_b;
  int early_r;
  int mid_b;
  int mid_r;

  initial begin
    pkg::make_double(21, out_b);
    inout_v = 5;
    pkg::bump(inout_v);
    pkg::ref_set(ref_r, 77);
    cref_in = 8;
    cref_out = pkg::const_ref_read(cref_in);
    quotient = pkg::div_mod(17, 5, rem);
    pkg::delayed_out(1, task_out);
    nested_ret = pkg::scaled(4, nested_o) * 2;
  end

  initial begin
    early_b = 3;
    early_r = 200;
    pkg::early_out(1, early_b, early_r);
  end

  initial begin
    #2;
    mid_b = early_b;
    mid_r = early_r;
  end

  final begin
    if (out_b !== 42) $fatal(1, "out_b was %0d, expected 42", out_b);
    if (inout_v !== 15) $fatal(1, "inout_v was %0d, expected 15", inout_v);
    if (ref_r !== 77) $fatal(1, "ref_r was %0d, expected 77", ref_r);
    if (cref_in !== 8) $fatal(1, "cref_in was %0d, expected 8", cref_in);
    if (cref_out !== 9) $fatal(1, "cref_out was %0d, expected 9", cref_out);
    if (quotient !== 3) $fatal(1, "quotient was %0d, expected 3", quotient);
    if (rem !== 2) $fatal(1, "rem was %0d, expected 2", rem);
    if (task_out !== 101) $fatal(1, "task_out was %0d, expected 101", task_out);
    if (nested_ret !== 24)
      $fatal(1, "nested_ret was %0d, expected 24", nested_ret);
    if (nested_o !== 5) $fatal(1, "nested_o was %0d, expected 5", nested_o);
    if (mid_b !== 3) $fatal(1, "mid_b was %0d, expected 3", mid_b);
    if (mid_r !== 201) $fatal(1, "mid_r was %0d, expected 201", mid_r);
    if (early_b !== 101) $fatal(1, "early_b was %0d, expected 101", early_b);
    if (early_r !== 201) $fatal(1, "early_r was %0d, expected 201", early_r);
    $display("All checks passed");
  end
endmodule
