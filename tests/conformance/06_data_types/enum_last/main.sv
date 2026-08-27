// The last() method returns the value of the last member of the enumeration,
// which is the name declared last and not the name with the largest value. The
// value the variable happens to hold does not enter into it (LRM 6.19.5.2).
module Top;
  typedef enum {A = 10, B = 15, C = 5} t;

  t v;
  int from_a;
  int from_b;

  initial begin
    v = A;
    from_a = v.last();
    v = B;
    from_b = v.last();
  end

  final begin
    if (from_a !== 5) $fatal(1, "from_a was %0d, expected 5", from_a);
    if (from_b !== 5) $fatal(1, "from_b was %0d, expected 5", from_b);
    $display("All checks passed");
  end
endmodule
