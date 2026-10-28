// A process is a class object whose generator is the one its thread draws from
// (LRM 9.7, 18.13.3), and it declares no random variable, so randomizing it
// chooses nothing (LRM 18.6.1). The standard does not say whether a call
// choosing nothing still advances the generator, nor whether naming the
// running process does. Here neither does: the hooks, the mode methods,
// process::self() and a randomize() of the process, with or without an inline
// constraint, leave its next draw the one it would have made without them.
module Top;
  int unsigned undisturbed = 0;
  int unsigned after_calls = 1;
  int answered = -1;

  initial begin
    process p;
    p = process::self();

    p.srandom(5);
    undisturbed = $urandom;

    p.srandom(5);
    p = process::self();
    answered = p.randomize();
    answered += p.randomize() with { 1 == 1; };
    p.pre_randomize();
    p.post_randomize();
    p.rand_mode(1);
    p.constraint_mode(1);
    after_calls = $urandom;
  end

  final begin
    if (answered !== 2)
      $fatal(1, "randomizing a process answered %0d over two calls", answered);
    if (after_calls !== undisturbed)
      $fatal(1, "a process drew %0h after calls choosing nothing, %0h without",
             after_calls, undisturbed);
    $display("All checks passed");
  end
endmodule
