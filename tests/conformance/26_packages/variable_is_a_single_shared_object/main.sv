// A variable declared in a package is one object the package owns, and every
// scope naming it reads and writes that same object, whether the name is
// reached from inside the package or from another scope by a resolved name
// (LRM 26.2, 26.3). Its declaration assignment happens before any initial or
// always procedure is started (LRM 26.2), so the first read from another scope
// already sees the initialized value; and because it is an ordinary variable, a
// write to it is a value change a procedure elsewhere can synchronize on (LRM
// 9.4.2).
package pkg;
  int cnt = 5;

  function automatic int doubled();
    return cnt * 2;
  endfunction

  function automatic void bump();
    cnt = cnt + 1;
  endfunction

  function automatic void add(int n);
    cnt = cnt + n;
  endfunction

  function automatic void bump_twice();
    bump();
    bump();
  endfunction
endpackage

module Top;
  int init_read;
  int fn_read;
  int after_write;
  int after_bump;
  int after_add;
  int after_sibling;
  int mirror = 0;

  always @(pkg::cnt) mirror = pkg::cnt;

  initial begin
    init_read = pkg::cnt;
    fn_read = pkg::doubled();
    pkg::cnt = 7;
    after_write = pkg::cnt;
    pkg::bump();
    after_bump = pkg::cnt;
    pkg::add(4);
    after_add = pkg::cnt;
    pkg::bump_twice();
    after_sibling = pkg::cnt;
    #1;
  end

  final begin
    if (init_read !== 5) $fatal(1, "init_read was %0d, expected 5", init_read);
    if (fn_read !== 10) $fatal(1, "fn_read was %0d, expected 10", fn_read);
    if (after_write !== 7)
      $fatal(1, "after_write was %0d, expected 7", after_write);
    if (after_bump !== 8)
      $fatal(1, "after_bump was %0d, expected 8", after_bump);
    if (after_add !== 12)
      $fatal(1, "after_add was %0d, expected 12", after_add);
    if (after_sibling !== 14)
      $fatal(1, "after_sibling was %0d, expected 14", after_sibling);
    if (mirror !== 14) $fatal(1, "mirror was %0d, expected 14", mirror);
    $display("All checks passed");
  end
endmodule
