// process is a class (LRM 9.7), and every class has randomize(),
// pre_randomize() and post_randomize() (LRM 18.6.1, 18.6.2) and the mode
// methods (LRM 18.8, 18.9). A process declares no random variable and no
// constraint and cannot be extended, so its hooks do nothing and randomize()
// solves nothing: it answers 1, and with an inline constraint on a state
// variable it answers whether that constraint holds, leaving the variable as it
// was (LRM 18.7). Turning the object's constraints off leaves an inline one in
// force, since constraint_mode() acts on the blocks a class declares. The
// generator srandom() reaches is still the process's own (LRM 18.13.3).
module Top;
  int x = 0;
  int plain = -1;
  int holds = -1;
  int fails = -1;
  int after_modes = -1;
  int x_after = -1;
  int unsigned drawn_first = 0;
  int unsigned drawn_again = 1;

  initial begin
    process p;
    p = process::self();

    plain = p.randomize();
    holds = p.randomize() with { x == 0; };
    fails = p.randomize() with { x == 1; };
    x_after = x;

    p.pre_randomize();
    p.post_randomize();
    p.rand_mode(0);
    p.constraint_mode(0);
    after_modes = p.randomize() with { x == 1; };

    p.srandom(11);
    drawn_first = $urandom;
    p.srandom(11);
    drawn_again = $urandom;
  end

  final begin
    if (plain !== 1)
      $fatal(1, "a process's randomize() answered %0d", plain);
    if (holds !== 1)
      $fatal(1, "a process's randomize() with a holding constraint answered %0d",
             holds);
    if (fails !== 0)
      $fatal(1, "a process's randomize() with a failing constraint answered %0d",
             fails);
    if (x_after !== 0)
      $fatal(1, "a process's randomize() changed the state variable to %0d",
             x_after);
    if (after_modes !== 0)
      $fatal(1, "constraint_mode(0) on a process turned off an inline constraint");
    if (drawn_first !== drawn_again)
      $fatal(1, "a reseeded process drew %0h then %0h", drawn_first, drawn_again);
    $display("All checks passed");
  end
endmodule
