// A $unit-scope declaration (LRM 3.12.1) lies outside any design element and is
// reached from a design element by a bare name -- the same cross-unit by-name
// form a package member uses. The $unit scope is modeled as an anonymous
// compilation unit (a namespace unit with no source name of its own), so a
// variable (read and write), a function, and a task declared there behave
// exactly as their package-scoped counterparts. This exercises all three real
// cross-unit entities; compile-time contents (parameters, typedefs) fold in
// place and need no unit, covered elsewhere.
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
endmodule
