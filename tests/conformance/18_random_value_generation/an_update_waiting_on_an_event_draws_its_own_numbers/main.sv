// A nonblocking assignment carrying an event control leaves the update to an
// execution the standard makes no process of (LRM 9.4.5), which still
// evaluates the control: where the statement stands, and again at each change
// that might be its event. What it draws there comes from a generator of its
// own, seeded from the next random value of the procedure that reached the
// statement, as a thread's is (LRM 18.14.1, 18.14.2).
//
// So two such updates reached by two procedures draw apart, and the reaching
// procedure's own stream moves by one at the statement and not by however
// often the control is evaluated afterwards: two instances of one module start
// from the same seed (LRM 18.14.1) and differ only in how many changes the
// control is asked about, and what each procedure draws after the statement
// matches the other's.
module Issuer #(
    parameter int Candidacies = 1
);
  int s = 0;
  bit done = 0;
  int a;
  int b;
  int unsigned first_a;
  int unsigned first_b;
  bit drew_a = 0;
  bit drew_b = 0;
  int unsigned after_a;

  function automatic int for_a(int x);
    int unsigned drawn = $urandom;
    if (!drew_a) first_a = drawn;
    drew_a = 1;
    return x;
  endfunction

  function automatic int for_b(int x);
    int unsigned drawn = $urandom;
    if (!drew_b) first_b = drawn;
    drew_b = 1;
    return x;
  endfunction

  initial begin
    a <= @(for_a(s) iff done) 1;
    after_a = $urandom;
  end

  initial b <= @(for_b(s) iff done) 1;

  initial begin
    repeat (Candidacies) #1 s = s + 1;
    #1 done = 1;
    s = s + 1;
  end
endmodule

module Top;
  Issuer #(.Candidacies(1)) once ();
  Issuer #(.Candidacies(5)) often ();

  final begin
    if (once.a !== 1 || once.b !== 1 || often.a !== 1 || often.b !== 1)
      $fatal(1, "an update did not land");
    if (once.first_a === once.first_b)
      $fatal(1, "two procedures' updates drew the same value %0h",
             once.first_a);
    if (once.first_a !== often.first_a)
      $fatal(1, "updates from one seed drew %0h and %0h, expected the same",
             once.first_a, often.first_a);
    if (once.after_a !== often.after_a)
      $fatal(1, "the procedure drew %0h and %0h after the statement, expected the same",
             once.after_a, often.after_a);
    $display("All checks passed");
  end
endmodule
