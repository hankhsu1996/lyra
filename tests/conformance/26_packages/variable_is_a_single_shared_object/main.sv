// A variable declared in a package is one object the package owns, and every
// scope naming it reads and writes that same object, whether the name is
// reached from inside the package or from another scope by a resolved name
// (LRM 26.2, 26.3). Its declaration assignment happens before any initial or
// always procedure is started (LRM 26.2), so the first read from another scope
// already sees the initialized value; and because it is an ordinary variable, a
// write to it is a value change a procedure elsewhere can synchronize on (LRM
// 9.4.2). Being ordinary also means every assignment form reaches it: a
// nonblocking write to it defers exactly as it would to any other variable, so
// a read taken after the statement in the same time step still sees the old
// value and the assigned one appears only once the update region has run (LRM
// 10.4.2).
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
  int nba_same_step;
  int nba_next_step;
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
    pkg::cnt <= 20;
    nba_same_step = pkg::cnt;
    #1;
    nba_next_step = pkg::cnt;
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
    if (nba_same_step !== 14)
      $fatal(1, "nba_same_step was %0d, expected 14", nba_same_step);
    if (nba_next_step !== 20)
      $fatal(1, "nba_next_step was %0d, expected 20", nba_next_step);
    if (mirror !== 20) $fatal(1, "mirror was %0d, expected 20", mirror);
    $display("All checks passed");
  end
endmodule
