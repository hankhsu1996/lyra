// @rejected:
//
// A branch its fork does not wait for may run after the call that passed a
// `ref` argument has returned, so it shall not name the argument unless the
// argument is `ref static`, and a conforming tool rejects this program (LRM
// 9.3.2).
module Top;
  task automatic set_later(ref int r);
    fork
      #1 r = 3;
    join_none
  endtask

  task automatic caller();
    int x;
    set_later(x);
  endtask

  initial begin
    caller();
    #2 $display("the program the standard rejects ran");
  end
endmodule
