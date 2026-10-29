// @rejected:
//
// `this` "shall only be used as type(this) ... or within non-static class
// methods, constraints, inlined constraint methods, or covergroups embedded
// within classes ...; otherwise, an error shall be issued", and a property's
// initializer is none of those, so a conforming tool rejects this program (LRM
// 8.11).
module Top;
  class C;
    int a = 1;
    int b = this.a;
  endclass

  initial begin
    static C c = new;
    $display("the program the standard rejects ran with %0d", c.b);
  end
endmodule
