// @rejected:
//
// A formal passed by reference binds what its default names wherever a call
// omits it, and a member of an unpacked union is not among what may be passed
// by reference, so a conforming tool rejects this program (LRM 13.5.2,
// 13.5.3).
module Top;
  typedef union { int a; int b; } u_t;
  u_t v;

  task automatic set(ref int r = v.a);
    r = 3;
  endtask

  initial begin
    set();
    $display("the program the standard rejects ran");
  end
endmodule
