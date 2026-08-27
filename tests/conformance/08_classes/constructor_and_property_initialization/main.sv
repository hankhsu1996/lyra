// Creating an object runs the class constructor. Each property first takes
// the value its declaration gives, or its type's uninitialized value where
// the declaration gives none, in declaration order -- so an initializer may
// read a property declared before it -- and only then does the body of a
// user-defined constructor run. A class that declares no new function is
// given one, which has no effect beyond that property initialization. A
// constructor's arguments follow the conventions of any other subroutine
// call, default argument values included (LRM 8.7).
module Top;
  class WithCtor;
    int x = 5;
    int y = x + 1;
    int z;
    int w;

    function new(int a, int b = 20);
      z = a + y;
      w = b;
    endfunction
  endclass

  class NoCtor;
    int p = 7;
    int q = p + p;
    int r;
  endclass

  int one_x;
  int one_y;
  int one_z;
  int one_w;
  int two_z;
  int two_w;
  int plain_p;
  int plain_q;
  int plain_r;

  initial begin
    WithCtor one;
    WithCtor two;
    NoCtor plain;

    plain_r = 6;

    one = new(10);
    two = new(1, 2);
    plain = new;

    one_x = one.x;
    one_y = one.y;
    one_z = one.z;
    one_w = one.w;
    two_z = two.z;
    two_w = two.w;
    plain_p = plain.p;
    plain_q = plain.q;
    plain_r = plain.r;
  end

  final begin
    if (one_x !== 5) $fatal(1, "one_x was %0d, expected 5", one_x);
    if (one_y !== 6) $fatal(1, "one_y was %0d, expected 6", one_y);
    if (one_z !== 16) $fatal(1, "one_z was %0d, expected 16", one_z);
    if (one_w !== 20) $fatal(1, "one_w was %0d, expected 20", one_w);
    if (two_z !== 7) $fatal(1, "two_z was %0d, expected 7", two_z);
    if (two_w !== 2) $fatal(1, "two_w was %0d, expected 2", two_w);
    if (plain_p !== 7) $fatal(1, "plain_p was %0d, expected 7", plain_p);
    if (plain_q !== 14) $fatal(1, "plain_q was %0d, expected 14", plain_q);
    if (plain_r !== 0) $fatal(1, "plain_r was %0d, expected 0", plain_r);
    $display("All checks passed");
  end
endmodule
