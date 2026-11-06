// @rejected:
//
// Only a variable, a class property, a member of an unpacked structure and an
// element of an unpacked array may be passed by reference, so a member of an
// unpacked union is none of them and a conforming tool rejects this program
// (LRM 13.5.2).
module Top;
  typedef union { int a; int b; } u_t;
  u_t v;

  task automatic set(ref int r);
    r = 3;
  endtask

  initial begin
    set(v.a);
    $display("the program the standard rejects ran");
  end
endmodule
