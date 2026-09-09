// $random returns a new 32-bit signed random number on each call, and its seed
// argument is an integral variable the draw advances, so restoring the seed
// replays the stream (LRM 20.14.1). The generation algorithm is part of the
// standard rather than left to the implementation, which is what makes the
// seeds below a requirement any conforming simulator meets and not a recording
// of one simulator's choice: Annex N states the algorithm as C source, and its
// Table N.1 gives $random as the uniform draw bounded by the whole signed
// range, so the two spellings are one function and advance a seed alike.
module Top;
  integer seed;
  integer uniform_seed;
  int drawn;
  int as_random;
  int as_uniform;

  final begin
    seed = 1;
    drawn = $random(seed);
    drawn = $random(seed);
    drawn = $random(seed);
    if (seed !== -1017563188)
      $fatal(1, "seed after three draws from 1 was %0d, expected -1017563188",
             seed);

    seed = 7;
    drawn = $random(seed);
    drawn = $random(seed);
    drawn = $random(seed);
    if (seed !== -1386778934)
      $fatal(1, "seed after three draws from 7 was %0d, expected -1386778934",
             seed);

    // Restoring the seed replays the stream, which is what makes the seed the
    // caller's to own rather than the simulator's.
    seed = 12345;
    drawn = $random(seed);
    drawn = $random(seed);
    drawn = $random(seed);
    if (seed !== 1023442532)
      $fatal(1, "seed after three draws from 12345 was %0d, expected 1023442532",
             seed);
    seed = 12345;
    drawn = $random(seed);
    drawn = $random(seed);
    drawn = $random(seed);
    if (seed !== 1023442532)
      $fatal(1, "the replayed run left the seed at %0d, expected 1023442532",
             seed);

    // Table N.1: the draw is the uniform one over the whole signed range, so
    // the two spellings answer alike and leave the same seed behind.
    seed = 99;
    uniform_seed = 99;
    as_random = $random(seed);
    as_uniform = $dist_uniform(uniform_seed, -2147483648, 2147483647);
    if (as_uniform !== as_random)
      $fatal(1, "the uniform draw over the whole range gave %0d, $random %0d",
             as_uniform, as_random);
    if (uniform_seed !== seed)
      $fatal(1, "the two spellings left seeds %0d and %0d", uniform_seed, seed);

    $display("All checks passed");
  end
endmodule
