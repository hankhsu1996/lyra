// A package subroutine (LRM 26.3) reached across the unit boundary carries its
// output / inout / ref formals back to the caller's actuals exactly as an
// intra-unit call does (LRM 13.5). output and inout ride the callee's
// completion payload, written back after the call; ref / const ref alias the
// caller's cell. This exercises every by-reference direction, a function whose
// return value and an output argument are written back together, and a task
// whose output crosses a suspension -- all against a package callee.
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
endmodule
