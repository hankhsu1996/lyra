// The first() method returns the value of the first member of the enumeration,
// which is the name declared first and not the name with the smallest value.
// The value the variable happens to hold does not enter into it
// (LRM 6.19.5.1).
module Top;
  typedef enum {A = 10, B = 15, C = 5} t;

  t v;
  int from_b;
  int from_c;

  initial begin
    v = B;
    from_b = v.first();
    v = C;
    from_c = v.first();
  end

  final begin
    if (from_b !== 10) $fatal(1, "from_b was %0d, expected 10", from_b);
    if (from_c !== 10) $fatal(1, "from_c was %0d, expected 10", from_c);
    $display("All checks passed");
  end
endmodule
