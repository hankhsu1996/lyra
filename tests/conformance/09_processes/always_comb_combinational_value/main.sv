// An always_comb procedure leaves its targets holding the combinational
// function of the variables it reads, whatever shape the body takes: a single
// assignment, a statement group assigning several targets, or a conditional
// that assigns the same target from either arm (LRM 9.2.2.2). Where one
// procedure's target is a variable another procedure reads, the second has that
// variable in its implicit sensitivity list and re-evaluates when the first
// writes it, so a cascade settles to a consistent result (LRM 9.2.2.2.1).
module Top;
  int a;
  int b;
  bit sel;

  int single;
  int sum;
  int prod;
  int taken;
  int not_taken;
  int doubled;
  int tripled;

  always_comb single = a + b;

  always_comb begin
    sum = a + b;
    prod = a * b;
  end

  always_comb begin
    if (sel) taken = b;
    else taken = a;
  end

  always_comb begin
    if (!sel) not_taken = b;
    else not_taken = a;
  end

  always_comb doubled = a * 2;
  always_comb tripled = doubled * 3;

  initial begin
    a = 4;
    b = 7;
    sel = 1;
  end

  final begin
    if (single !== 11) $fatal(1, "single was %0d, expected 11", single);
    if (sum !== 11) $fatal(1, "sum was %0d, expected 11", sum);
    if (prod !== 28) $fatal(1, "prod was %0d, expected 28", prod);
    if (taken !== 7) $fatal(1, "taken was %0d, expected 7", taken);
    if (not_taken !== 4) $fatal(1, "not_taken was %0d, expected 4", not_taken);
    if (doubled !== 8) $fatal(1, "doubled was %0d, expected 8", doubled);
    if (tripled !== 24) $fatal(1, "tripled was %0d, expected 24", tripled);
    $display("All checks passed");
  end
endmodule
