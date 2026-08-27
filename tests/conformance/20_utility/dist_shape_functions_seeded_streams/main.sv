// The six shaped probabilistic distribution functions each return an integer
// drawn from the distribution their name states, taking their seed as an inout
// argument the draw advances (LRM 20.14.2). Their generation algorithm is part
// of the standard rather than left to the implementation -- Annex N states it
// as C source -- so the values below are what any conforming simulator produces
// from these seeds, not a recording of one simulator's choice.
module Top;
  int seed;
  int drawn;

  final begin
    seed = 1;

    drawn = -1;
    drawn = $dist_normal(seed, 100, 10);
    if (drawn !== 106)
      $fatal(1, "first normal draw was %0d, expected 106", drawn);

    drawn = -1;
    drawn = $dist_normal(seed, 100, 10);
    if (drawn !== 76)
      $fatal(1, "second normal draw was %0d, expected 76", drawn);

    drawn = -1;
    drawn = $dist_normal(seed, 100, 10);
    if (drawn !== 102)
      $fatal(1, "third normal draw was %0d, expected 102", drawn);

    if (seed !== 797919023)
      $fatal(1, "seed after normal was %0d, expected 797919023", seed);

    seed = 1;

    drawn = -1;
    drawn = $dist_exponential(seed, 5);
    if (drawn !== 55)
      $fatal(1, "first exponential draw was %0d, expected 55", drawn);

    drawn = -1;
    drawn = $dist_exponential(seed, 5);
    if (drawn !== 11)
      $fatal(1, "second exponential draw was %0d, expected 11", drawn);

    drawn = -1;
    drawn = $dist_exponential(seed, 5);
    if (drawn !== 1)
      $fatal(1, "third exponential draw was %0d, expected 1", drawn);

    if (seed !== -1017563188)
      $fatal(1, "seed after exponential was %0d, expected -1017563188", seed);

    seed = 1;

    drawn = -1;
    drawn = $dist_poisson(seed, 4);
    if (drawn !== 0)
      $fatal(1, "first poisson draw was %0d, expected 0", drawn);

    drawn = -1;
    drawn = $dist_poisson(seed, 4);
    if (drawn !== 2)
      $fatal(1, "second poisson draw was %0d, expected 2", drawn);

    drawn = -1;
    drawn = $dist_poisson(seed, 4);
    if (drawn !== 6)
      $fatal(1, "third poisson draw was %0d, expected 6", drawn);

    if (seed !== -1551342684)
      $fatal(1, "seed after poisson was %0d, expected -1551342684", seed);

    seed = 1;

    drawn = -1;
    drawn = $dist_chi_square(seed, 3);
    if (drawn !== 1)
      $fatal(1, "first chi-square draw was %0d, expected 1", drawn);

    drawn = -1;
    drawn = $dist_chi_square(seed, 3);
    if (drawn !== 2)
      $fatal(1, "second chi-square draw was %0d, expected 2", drawn);

    drawn = -1;
    drawn = $dist_chi_square(seed, 3);
    if (drawn !== 1)
      $fatal(1, "third chi-square draw was %0d, expected 1", drawn);

    if (seed !== -1551342684)
      $fatal(1, "seed after chi-square was %0d, expected -1551342684", seed);

    seed = 1;

    drawn = 99;
    drawn = $dist_t(seed, 5);
    if (drawn !== -6) $fatal(1, "first t draw was %0d, expected -6", drawn);

    drawn = 99;
    drawn = $dist_t(seed, 5);
    if (drawn !== 0) $fatal(1, "second t draw was %0d, expected 0", drawn);

    drawn = 99;
    drawn = $dist_t(seed, 5);
    if (drawn !== 2) $fatal(1, "third t draw was %0d, expected 2", drawn);

    if (seed !== 1692139853)
      $fatal(1, "seed after t was %0d, expected 1692139853", seed);

    seed = 1;

    drawn = -1;
    drawn = $dist_erlang(seed, 3, 10);
    if (drawn !== 45)
      $fatal(1, "first erlang draw was %0d, expected 45", drawn);

    drawn = -1;
    drawn = $dist_erlang(seed, 3, 10);
    if (drawn !== 6)
      $fatal(1, "second erlang draw was %0d, expected 6", drawn);

    drawn = -1;
    drawn = $dist_erlang(seed, 3, 10);
    if (drawn !== 6)
      $fatal(1, "third erlang draw was %0d, expected 6", drawn);

    if (seed !== -505977370)
      $fatal(1, "seed after erlang was %0d, expected -505977370", seed);

    $display("All checks passed");
  end
endmodule
