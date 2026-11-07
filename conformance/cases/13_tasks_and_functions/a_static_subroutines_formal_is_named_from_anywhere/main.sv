// A task or function a package or the compilation-unit scope declares without
// the automatic keyword is static, with all its declared items statically
// allocated -- its input, output and inout formals included -- so each formal
// is one variable every call shares, holds its type's default until a call
// writes it, and keeps its value from one call to the next (LRM 13.3.1,
// 13.3.2, 13.4.2). A hierarchical name reaches any static variable not in an
// unnamed block (LRM 6.21), so the formal is named through its subroutine --
// `step.a`, `fp::twice.b`, `unit_step.u` -- from the subroutine itself, from a
// sibling, from a class of the package, from another package and from a
// module, and every such name is that one variable: read, written, waited on
// and read by a continuous assignment. A call writes what it hands in to it
// and takes what it hands back from it, so a write made between two calls is
// what an output the second call leaves alone hands back, and the second
// call's input replaces what was written. A formal named like a variable of
// its package is a variable of its own.
package fp;
  int a = -1;
  int forked_sum;

  task step(input int a, output int o, inout int io);
    #1;
    o = step.a + 1;
    io = io + a;
  endtask

  function int twice(int b);
    return b * 2;
  endfunction

  function int b_seen();
    return twice.b;
  endfunction

  function void b_set(int v);
    twice.b = v;
  endfunction

  function int defaulted(int d = 5);
    return d;
  endfunction

  function int at_startup(int i);
    return i;
  endfunction

  int started = at_startup(3);

  function void never_called(int n, logic l);
  endfunction

  typedef struct {
    int x;
    int y;
  } pair_t;

  function void keep(string s, pair_t p, int arr[2]);
  endfunction

  task maybe(output int o, input bit write);
    if (write) o = 7;
  endtask

  task overlap(input int a, output int o);
    #10 o = a;
  endtask

  task spawn(int given);
    fork
      #10 forked_sum += given;
    join_none
  endtask

  class Reader;
    function int seen();
      return twice.b;
    endfunction
  endclass
endpackage

package fq;
  function int b_through_another_package();
    return fp::twice.b;
  endfunction
endpackage

package automatic fa;
  function static int kept(int k);
    return k;
  endfunction
endpackage

task unit_step(input int u, output int o);
  o = unit_step.u + 1;
endtask

function int unit_seen();
  return unit_step.u;
endfunction

interface Ifc;
  task it(input int a);
  endtask
endinterface

module Top;
  Ifc ifc ();

  int mirrored;
  assign mirrored = fp::twice.b;

  int woke_at = -1;
  int default_n = -1;
  int default_before_a_call = -1;
  logic default_l = 1'b0;
  int started_with = -1;
  int stepped_o = -1;
  int stepped_io = 10;
  int stepped_a = -1;
  int written = -1;
  int left_over = -1;
  int b_after_call = -1;
  int b_after_sibling_write = -1;
  int b_path_after_sibling_write = -1;
  int b_from_a_class = -1;
  int b_from_another_package = -1;
  int b_after_next_call = -1;
  int defaulted_value = -1;
  int defaulted_formal = -1;
  string kept_s = "";
  int kept_y = -1;
  int kept_element = -1;
  int automatic_package_k = -1;
  int unit_o = -1;
  int unit_from_a_sibling = -1;
  int unit_from_outside = -1;
  int interface_a = -1;
  int package_a = 0;
  int shared_first = -1;
  int shared_second = -1;

  initial begin
    @(fp::step.a);
    woke_at = fp::step.a;
  end

  initial fp::overlap(1, shared_first);
  initial #1 fp::overlap(2, shared_second);

  initial begin
    static fp::Reader reader = new;

    default_n = fp::never_called.n;
    default_l = fp::never_called.l;
    default_before_a_call = fp::defaulted.d;
    started_with = fp::at_startup.i;

    #1;
    fp::step(5, stepped_o, stepped_io);
    stepped_a = fp::step.a;

    fp::maybe(written, 1);
    fp::maybe.o = 9;
    fp::maybe(left_over, 0);

    void'(fp::twice(4));
    b_after_call = fp::b_seen();
    fp::b_set(11);
    b_after_sibling_write = fp::b_seen();
    b_path_after_sibling_write = fp::twice.b;
    b_from_a_class = reader.seen();
    b_from_another_package = fq::b_through_another_package();
    void'(fp::twice(2));
    b_after_next_call = fp::twice.b;

    defaulted_value = fp::defaulted();
    defaulted_formal = fp::defaulted.d;

    fp::keep("hello", '{x: 1, y: 2}, '{3, 4});
    kept_s = fp::keep.s;
    kept_y = fp::keep.p.y;
    kept_element = fp::keep.arr[1];

    void'(fa::kept(8));
    automatic_package_k = fa::kept.k;

    unit_step(20, unit_o);
    unit_from_a_sibling = unit_seen();
    unit_from_outside = unit_step.u;

    ifc.it(6);
    interface_a = ifc.it.a;

    package_a = fp::a;

    fp::spawn(42);
    fp::spawn(7);
  end

  final begin
    if (default_n !== 0 || default_l !== 1'bx)
      $fatal(1, "an uncalled function's formals held %0d and %b, expected 0 and x", default_n,
             default_l);
    if (default_before_a_call !== 0)
      $fatal(1, "a formal with a default held %0d before any call, expected 0",
             default_before_a_call);
    if (started_with !== 3)
      $fatal(1, "a call made by a package variable's initializer left %0d, expected 3",
             started_with);
    if (woke_at !== 5) $fatal(1, "a wait on a task's formal woke at %0d, expected 5", woke_at);
    if (stepped_o !== 6) $fatal(1, "the task naming its own input by path made %0d, expected 6",
                                stepped_o);
    if (stepped_io !== 15) $fatal(1, "the inout came back %0d, expected 15", stepped_io);
    if (stepped_a !== 5) $fatal(1, "a module read the task's input as %0d, expected 5", stepped_a);
    if (written !== 7) $fatal(1, "an output the call wrote came back %0d, expected 7", written);
    if (left_over !== 9)
      $fatal(1, "an output the call left alone came back %0d, expected the 9 a module wrote",
             left_over);
    if (b_after_call !== 4) $fatal(1, "a sibling read %0d after a call, expected 4", b_after_call);
    if (b_after_sibling_write !== 11 || b_path_after_sibling_write !== 11)
      $fatal(1, "after a sibling wrote 11 it read %0d, and a module %0d", b_after_sibling_write,
             b_path_after_sibling_write);
    if (b_from_a_class !== 11)
      $fatal(1, "a class of the package read %0d, expected 11", b_from_a_class);
    if (b_from_another_package !== 11)
      $fatal(1, "another package read %0d, expected 11", b_from_another_package);
    if (b_after_next_call !== 2)
      $fatal(1, "the next call left %0d, expected its own 2", b_after_next_call);
    if (mirrored !== 2)
      $fatal(1, "a continuous assignment of the formal held %0d, expected 2", mirrored);
    if (defaulted_value !== 5 || defaulted_formal !== 5)
      $fatal(1, "an omitted argument gave %0d and left %0d, expected 5 and 5", defaulted_value,
             defaulted_formal);
    if (kept_s != "hello" || kept_y !== 2 || kept_element !== 4)
      $fatal(1, "a string, structure and array formal held \"%s\", %0d and %0d", kept_s, kept_y,
             kept_element);
    if (automatic_package_k !== 8)
      $fatal(1, "a static function of an automatic package held %0d, expected 8",
             automatic_package_k);
    if (unit_o !== 21 || unit_from_a_sibling !== 20 || unit_from_outside !== 20)
      $fatal(1, "a compilation-unit task made %0d, and its input read %0d and %0d", unit_o,
             unit_from_a_sibling, unit_from_outside);
    if (interface_a !== 6)
      $fatal(1, "an interface task's input read %0d, expected 6", interface_a);
    if (package_a !== -1)
      $fatal(1, "the package variable named like a formal held %0d, expected -1", package_a);
    if (shared_first !== 2 || shared_second !== 2)
      $fatal(1, "a package task's overlapping calls returned %0d and %0d, expected 2 and 2",
             shared_first, shared_second);
    if (fp::forked_sum !== 14)
      $fatal(1, "the branches a package task forked added up to %0d, expected 7 twice",
             fp::forked_sum);
    $display("All checks passed");
  end
endmodule
