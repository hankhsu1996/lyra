// An import declaration provides direct visibility of a package's identifiers
// in the importing scope, so they are usable there without the package
// qualifier: the explicit form imports only the identifiers it names, and the
// wildcard form offers every identifier the package declares (LRM 26.3).
// Importing does not copy the declaration, so a bare name denotes the very
// object the package declared -- a write through it lands in the package's
// variable, where the resolved name reads it back, and a call through it enters
// the package's own subroutine, task as well as function. An explicit import
// may sit beside a wildcard import of the same package, since importing an
// identifier from the same package more than once is allowed (LRM 26.3). A
// scope may import from several packages at once, and where no wildcard import
// offers an identifier, naming it in an explicit import is what makes it
// directly visible.
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

package other_pkg;
  int marker = 3;
endpackage

module Top;
  import pkg::cnt;
  import pkg::set_cnt;
  import pkg::*;
  import other_pkg::marker;

  int explicit_read;
  int var_read;
  int fn_read;
  int after_write;
  int resolved_read;
  int after_task;

  initial begin
    explicit_read = marker;
    var_read = cnt;
    fn_read = doubled();
    cnt = 8;
    after_write = cnt;
    resolved_read = pkg::cnt;
    set_cnt(20);
    after_task = cnt;
  end

  final begin
    if (explicit_read !== 3)
      $fatal(1, "explicit_read was %0d, expected 3", explicit_read);
    if (var_read !== 5) $fatal(1, "var_read was %0d, expected 5", var_read);
    if (fn_read !== 10) $fatal(1, "fn_read was %0d, expected 10", fn_read);
    if (after_write !== 8)
      $fatal(1, "after_write was %0d, expected 8", after_write);
    if (resolved_read !== 8)
      $fatal(1, "resolved_read was %0d, expected 8", resolved_read);
    if (after_task !== 20)
      $fatal(1, "after_task was %0d, expected 20", after_task);
    $display("All checks passed");
  end
endmodule
