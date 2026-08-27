// Every declaration lying outside any other scope belongs to the
// compilation-unit scope, which is not a package but may hold any item a
// package may hold (LRM 3.12.1, 26.2). A design element compiled in that unit
// reaches such a declaration by its simple name, and the name denotes one
// object: a write through it is seen by a later read, a function declared there
// reads it, and a task declared there writes it.
int cnt = 5;

function automatic int doubled();
  return cnt * 2;
endfunction

task automatic set_cnt(int v);
  #1;
  cnt = v;
endtask

module Top;
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

  final begin
    if (var_read !== 5) $fatal(1, "var_read was %0d, expected 5", var_read);
    if (fn_read !== 10) $fatal(1, "fn_read was %0d, expected 10", fn_read);
    if (after_write !== 8)
      $fatal(1, "after_write was %0d, expected 8", after_write);
    if (after_task !== 20)
      $fatal(1, "after_task was %0d, expected 20", after_task);
    $display("All checks passed");
  end
endmodule
