// An enumerated value used in an expression is cast automatically to the
// enumeration's base type. It therefore contributes the number its name stands
// for to arithmetic, the bits of that base type and no more to a
// concatenation, and compares equal to exactly the name it holds
// (LRM 6.19.3, 6.19.4).
module Top;
  typedef enum {A = 10, B = 20, C = 30} val_t;
  typedef enum logic [6:0] {OP = 7'h33} op_t;

  val_t v;
  int sum;
  logic [8:0] joined;
  int eq_same;
  int eq_other = -1;
  int ne_other;

  initial begin
    v = A;
    sum = v + 5;
    joined = {2'b10, OP};

    v = B;
    eq_same = (v == B);
    eq_other = (v == A);
    ne_other = (v != C);
  end

  final begin
    if (sum !== 15) $fatal(1, "sum was %0d, expected 15", sum);
    if (joined !== 9'h133) $fatal(1, "joined was %0h, expected 133", joined);
    if (eq_same !== 1) $fatal(1, "eq_same was %0d, expected 1", eq_same);
    if (eq_other !== 0) $fatal(1, "eq_other was %0d, expected 0", eq_other);
    if (ne_other !== 1) $fatal(1, "ne_other was %0d, expected 1", ne_other);
    $display("All checks passed");
  end
endmodule
