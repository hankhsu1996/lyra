// The algorithm $random generates by is part of the standard, "ensuring
// repeatable random value sets across different implementations" (LRM 20.14),
// and Annex N Table N.1 gives it as the uniform draw over the whole signed
// range advancing a seed. A call that passes no seed (LRM 20.14.1 makes the
// argument optional) is that same function, so its values are the ones a
// seeded call would give from wherever its stream stands.
//
// The standard does not say where that stream starts. It starts at a seed of
// zero here, once for the whole run, which Annex N's own code replaces with
// 259341593 before its first step; the values below are what that code gives
// from there, worked out from the text of the annex.
module Top;
  integer seed;
  int drawn;
  int seeded;

  final begin
    drawn = 0;
    drawn = $random;
    if (drawn !== 303379748)
      $fatal(1, "the first draw with no seed was %0d, expected 303379748",
             drawn);
    drawn = $random;
    if (drawn !== -1064739199)
      $fatal(1, "the second draw with no seed was %0d, expected -1064739199",
             drawn);

    // A seeded call in between advances the variable it was given and leaves
    // the run's stream where it was.
    seed = 0;
    seeded = 0;
    seeded = $random(seed);
    if (seeded !== 303379748)
      $fatal(1, "the first draw from a seed of 0 was %0d, expected 303379748",
             seeded);

    drawn = $random;
    if (drawn !== -2071669239)
      $fatal(1, "the third draw with no seed was %0d, expected -2071669239",
             drawn);
    drawn = $random;
    if (drawn !== -1309649309)
      $fatal(1, "the fourth draw with no seed was %0d, expected -1309649309",
             drawn);

    $display("All checks passed");
  end
endmodule
