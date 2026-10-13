// A min:typ:max expression can stand wherever an expression can, and its value
// is one of its three expressions: every triplet in an expression takes the
// same one, so the minimum of `(a:b:c) + (d:e:f)` is a+d, its typical value b+e
// and its maximum c+f (LRM 11.11).
module Top;
  int chosen = -1;

  initial chosen = (1 : 2 : 3) + (10 : 20 : 30);

  final begin
    if (chosen !== 11 && chosen !== 22 && chosen !== 33)
      $fatal(1, "chosen was %0d, expected 11, 22 or 33", chosen);
    $display("All checks passed");
  end
endmodule
