// An `import` brings a package's names into a scope's lookup (LRM 26.3); it does
// not create a new kind of reference. A name reached by a bare identifier after
// import resolves to the same cross-unit package entity as the explicit
// `pkg::item` form, so it lowers identically. This exercises both import forms
// against the entities that are real cross-unit symbols -- a package variable
// (read and write), a package function, and a package task -- since compile-time
// contents (parameters, typedefs, enums) fold or intern in place and are covered
// elsewhere. The explicit import of a name coexists with a wildcard import of the
// package: the explicit import takes precedence and is not a collision.
package pkg;
  int cnt = 5;

  function automatic int doubled();
    return cnt * 2;
  endfunction

  task automatic set_cnt(int v);
    #1;
    cnt = v;
  endtask
endpackage

module Top;
  import pkg::cnt;
  import pkg::set_cnt;
  import pkg::*;

  int var_read;
  int fn_read;
  int after_write;
  int after_task;

  initial begin
    var_read = cnt;
    fn_read = doubled();
    cnt = 8;
    after_write = cnt;
    set_cnt(20);
    after_task = cnt;
  end
endmodule
