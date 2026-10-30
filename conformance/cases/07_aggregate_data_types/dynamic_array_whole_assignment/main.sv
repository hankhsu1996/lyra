// Assigning to a dynamic array resizes it to the number of elements the source
// has and then copies them across in left-to-right order, so the source may be
// a dynamic array of any size, longer or shorter than the target or empty, and
// the target ends up matching it. Each element is copied by value, so writing to
// the source afterwards leaves the target alone. A nonblocking assignment copies
// the same way, but it only schedules the copy: the target keeps its old size
// and its old elements until the update is applied at the end of the time step
// (LRM 7.6, 10.4.2).
module Top;
  int source [] = '{100, 200, 300, 400};
  int unset [];

  int grown [];
  int copied [];
  int emptied [];
  int nba_target [];
  int nba_source [] = '{70, 80, 90};

  int grown_size;
  int grown0 = 77;
  int grown3 = 77;

  int copied0 = 77;
  int source0_after_write = 77;

  int emptied_size_before;
  int emptied_size_after = 77;

  int nba_size_before;
  int nba_element_before = 77;
  int nba_size_after;
  int nba0 = 77;
  int nba2 = 77;

  initial begin
    grown = new[2];
    grown[0] = 1;
    grown[1] = 2;
    grown = source;
    grown_size = grown.size();
    grown0 = grown[0];
    grown3 = grown[3];

    copied = source;
    source[0] = 999;
    copied0 = copied[0];
    source0_after_write = source[0];

    emptied = new[3];
    emptied[0] = 1;
    emptied_size_before = emptied.size();
    emptied = unset;
    emptied_size_after = emptied.size();

    nba_target = new[2];
    nba_target[0] = 1;
    nba_target[1] = 2;
    nba_target <= nba_source;
    nba_size_before = nba_target.size();
    nba_element_before = nba_target[0];
    #1;
    nba_size_after = nba_target.size();
    nba0 = nba_target[0];
    nba2 = nba_target[2];
  end

  final begin
    if (grown_size !== 4)
      $fatal(1, "grown_size was %0d, expected 4", grown_size);
    if (grown0 !== 100) $fatal(1, "grown0 was %0d, expected 100", grown0);
    if (grown3 !== 400) $fatal(1, "grown3 was %0d, expected 400", grown3);

    if (copied0 !== 100) $fatal(1, "copied0 was %0d, expected 100", copied0);
    if (source0_after_write !== 999)
      $fatal(1, "source0_after_write was %0d, expected 999",
             source0_after_write);

    if (emptied_size_before !== 3)
      $fatal(1, "emptied_size_before was %0d, expected 3",
             emptied_size_before);
    if (emptied_size_after !== 0)
      $fatal(1, "emptied_size_after was %0d, expected 0", emptied_size_after);

    if (nba_size_before !== 2)
      $fatal(1, "nba_size_before was %0d, expected 2", nba_size_before);
    if (nba_element_before !== 1)
      $fatal(1, "nba_element_before was %0d, expected 1", nba_element_before);
    if (nba_size_after !== 3)
      $fatal(1, "nba_size_after was %0d, expected 3", nba_size_after);
    if (nba0 !== 70) $fatal(1, "nba0 was %0d, expected 70", nba0);
    if (nba2 !== 90) $fatal(1, "nba2 was %0d, expected 90", nba2);
    $display("All checks passed");
  end
endmodule
