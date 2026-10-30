// Nothing in the language reserves a spelling to the implementation (LRM 5.6),
// so any word a tool composes for a scope, a handle or a flag of its own is a
// word some design also declares, and both keep their own meaning. A named
// block and a variable spelled after that block are two declarations in two
// name spaces (LRM 3.13); a design element spelled the way an enclosing
// element and a block inside it would be joined is a third; a variable spelled
// like a name a tool holds back for itself is a fourth; a package variable
// spelled like a record a tool keeps about a type is a fifth; a class spelled
// like a part a tool divides a unit's output into is a sixth, and a design
// element spelled like a program's own entry a seventh; and two block-local
// variables are two variables however either is spelled (LRM 6.21), including
// when one is spelled the way the other would be were it moved aside.
package pkg;
  int _lyra_packed_type_0 = 3;

  class opening;
    int v = 17;
  endclass

  class forward;
    int v = 19;
  endclass

  class body;
    int v = 23;
  endclass
endpackage

module main;
  int v;

  initial v = 29;
endmodule

module Top__foo;
  int v;

  initial v = 5;
endmodule

module Top;
  import pkg::*;

  int foo_borrowed_handle = 1;
  int foo__cancel_1 = 1;
  int body_0_borrowed_handle = 1;
  int sv_constant_0 = 1;
  int both_locals = 1;

  int from_classes = 0;

  Top__foo inst ();
  main entry ();

  initial begin
    automatic opening o = new;
    automatic forward f = new;
    automatic body b = new;
    from_classes = o.v + f.v + b.v;
  end

  initial begin : foo
    foo_borrowed_handle = 7;
    foo__cancel_1 = 9;
    body_0_borrowed_handle = 11;
    sv_constant_0 = 13;
  end

  initial begin
    automatic int self_2 = 100;
    automatic int self = 4;
    both_locals = self_2 + self;
  end

  final begin
    if (foo_borrowed_handle !== 7)
      $fatal(1, "foo_borrowed_handle was %0d, expected 7", foo_borrowed_handle);
    if (foo__cancel_1 !== 9)
      $fatal(1, "foo__cancel_1 was %0d, expected 9", foo__cancel_1);
    if (body_0_borrowed_handle !== 11)
      $fatal(
          1, "body_0_borrowed_handle was %0d, expected 11",
          body_0_borrowed_handle);
    if (sv_constant_0 !== 13)
      $fatal(1, "sv_constant_0 was %0d, expected 13", sv_constant_0);
    if (both_locals !== 104)
      $fatal(1, "both_locals was %0d, expected 104", both_locals);
    if (_lyra_packed_type_0 !== 3)
      $fatal(
          1, "_lyra_packed_type_0 was %0d, expected 3", _lyra_packed_type_0);
    if (from_classes !== 59)
      $fatal(1, "from_classes was %0d, expected 59", from_classes);
    if (entry.v !== 29) $fatal(1, "entry.v was %0d, expected 29", entry.v);
    if (inst.v !== 5) $fatal(1, "inst.v was %0d, expected 5", inst.v);
    $display("All checks passed");
  end
endmodule
