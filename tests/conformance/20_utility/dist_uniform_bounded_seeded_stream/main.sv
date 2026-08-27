// $dist_uniform returns a value uniformly distributed over the closed interval
// its two bounds describe, and its seed is an inout argument that the draw
// advances, so a stream is replayed by restoring the seed (LRM 20.14.2). The
// generation algorithm is part of the standard rather than left to the
// implementation, which is what makes the values below a requirement any
// conforming simulator meets and not a recording of one simulator's choice: the
// standard states the algorithm as C source in Annex N, and these are what it
// produces. A start that is not below the end yields the start and leaves the
// seed untouched.
module Top;
  integer seed;
  int drawn;

  final begin
    seed = 1;

    drawn = -1;
    drawn = $dist_uniform(seed, 0, 99);
    if (drawn !== 0)
      $fatal(1, "first draw from seed 1 was %0d, expected 0", drawn);

    drawn = -1;
    drawn = $dist_uniform(seed, 0, 99);
    if (drawn !== 11)
      $fatal(1, "second draw from seed 1 was %0d, expected 11", drawn);

    drawn = -1;
    drawn = $dist_uniform(seed, 0, 99);
    if (drawn !== 76)
      $fatal(1, "third draw from seed 1 was %0d, expected 76", drawn);

    if (seed !== -1017563188)
      $fatal(1, "seed after three draws was %0d, expected -1017563188", seed);

    // A negative start is as ordinary as any other, and a draw may land on a
    // bound itself rather than strictly inside.
    seed = 7;

    drawn = 99;
    drawn = $dist_uniform(seed, -3, 3);
    if (drawn !== -3)
      $fatal(1, "first draw over -3..3 was %0d, expected -3", drawn);

    drawn = 99;
    drawn = $dist_uniform(seed, -3, 3);
    if (drawn !== 2)
      $fatal(1, "second draw over -3..3 was %0d, expected 2", drawn);

    drawn = 99;
    drawn = $dist_uniform(seed, -3, 3);
    if (drawn !== 1)
      $fatal(1, "third draw over -3..3 was %0d, expected 1", drawn);

    if (seed !== -1386778934)
      $fatal(1, "seed after -3..3 was %0d, expected -1386778934", seed);

    // Restoring the seed replays the stream, which is the whole point of the
    // seed being an argument the caller owns.
    seed = 7;
    drawn = 99;
    drawn = $dist_uniform(seed, -3, 3);
    if (drawn !== -3) $fatal(1, "replayed draw was %0d, expected -3", drawn);

    seed = 12345;

    drawn = 0;
    drawn = $dist_uniform(seed, 10, 20);
    if (drawn !== 12)
      $fatal(1, "first draw over 10..20 was %0d, expected 12", drawn);

    drawn = 0;
    drawn = $dist_uniform(seed, 10, 20);
    if (drawn !== 19)
      $fatal(1, "second draw over 10..20 was %0d, expected 19", drawn);

    drawn = 0;
    drawn = $dist_uniform(seed, 10, 20);
    if (drawn !== 12)
      $fatal(1, "third draw over 10..20 was %0d, expected 12", drawn);

    if (seed !== 1023442532)
      $fatal(1, "seed after 10..20 was %0d, expected 1023442532", seed);

    // Bounds that describe no interval draw nothing at all.
    seed = 99;
    drawn = 0;
    drawn = $dist_uniform(seed, 50, 10);
    if (drawn !== 50) $fatal(1, "draw over 50..10 was %0d, expected 50", drawn);
    if (seed !== 99)
      $fatal(1, "seed moved to %0d over an empty interval", seed);

    $display("All checks passed");
  end
endmodule
