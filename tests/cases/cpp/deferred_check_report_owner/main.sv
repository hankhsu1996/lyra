// LRM 12.4.2.1: a pending violation report belongs to the process that raised
// it, so nothing coarser can stand in for that identity. Two procedures share
// Child's scope and two Child instances share the check site, so keying by
// either would merge reports the LRM keeps apart. The initializer of a static
// variable runs before any process exists (LRM 6.8), so the report it raises
// belongs to no process at all -- not an error: nothing can discard it, and it
// matures with the rest.
package pkg;
  function automatic int chk(int v);
    unique if (v > 0) ;
    else if (v > 3) ;
    return v;
  endfunction
endpackage

module Child;
  int a;
  int b;
  initial a = pkg::chk(5);
  initial b = pkg::chk(5);
endmodule

module Top;
  int unowned = pkg::chk(5);
  Child c0();
  Child c1();
endmodule
