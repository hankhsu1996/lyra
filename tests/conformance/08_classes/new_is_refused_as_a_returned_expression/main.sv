// @rejected:
//
// A class constructor call is a `class_new`, which the grammar admits only as
// the right-hand side of a variable declaration or of a blocking assignment
// (LRM A.2.4, A.6.2), and a return statement takes an expression (LRM A.6.5),
// which `new` is not, so a conforming tool rejects this program (LRM 8.7).
module Top;
  class C;
    int a = 1;
  endclass

  function automatic C make();
    return new;
  endfunction

  initial begin
    static C c = make();
    $display("the program the standard rejects ran with %0d", c.a);
  end
endmodule
