// A union is as large as its largest member (LRM 7.3), however large that is,
// and is a value like any other wherever it is held: a subroutine's local, an
// argument passed by value, a returned value. Here one member is an array of
// several thousand bytes, both in an untagged union and in a tagged one, and
// each is built, written through a member, copied in and out of a subroutine
// and read back.
module Top;
  typedef int words_t[1024];

  typedef union {
    words_t words;
    int first;
  } big_t;

  typedef union tagged {
    int Small;
    words_t Words;
  } tagged_big_t;

  function automatic big_t Doubled(big_t given);
    big_t local_big = given;
    foreach (local_big.words[i]) local_big.words[i] = 2 * local_big.words[i];
    return local_big;
  endfunction

  function automatic tagged_big_t Grown(tagged_big_t given);
    tagged_big_t local_big = given;
    if (local_big matches tagged Words .*)
      local_big.Words[1023] = local_big.Words[1023] + 1;
    return local_big;
  endfunction

  function automatic int SumOf(big_t given);
    int sum = 0;
    foreach (given.words[i]) sum += given.words[i];
    return sum;
  endfunction

  int doubled_last;
  int doubled_sum;
  int grown_last;
  int grown_first;
  int small_kept;

  initial begin
    big_t big;
    tagged_big_t tagged_big;
    words_t w;

    foreach (big.words[i]) big.words[i] = i;
    big = Doubled(big);
    doubled_last = big.words[1023];
    doubled_sum = SumOf(big);

    foreach (w[i]) w[i] = 5;
    tagged_big = tagged Words w;
    tagged_big = Grown(tagged_big);
    grown_last = tagged_big.Words[1023];
    grown_first = tagged_big.Words[0];

    tagged_big = tagged Small 7;
    tagged_big = Grown(tagged_big);
    small_kept = tagged_big.Small;
  end

  final begin
    if (doubled_last !== 2046)
      $fatal(1, "the doubled last word read %0d, expected 2046", doubled_last);
    if (doubled_sum !== 1047552)
      $fatal(1, "the doubled words summed to %0d, expected 1047552",
             doubled_sum);
    if (grown_last !== 6)
      $fatal(1, "the grown last word read %0d, expected 6", grown_last);
    if (grown_first !== 5)
      $fatal(1, "the grown first word read %0d, expected 5", grown_first);
    if (small_kept !== 7)
      $fatal(1, "the small member read %0d, expected 7", small_kept);
    $display("All checks passed");
  end
endmodule
