// A queue declared with no initial value holds no elements. One declared with
// an array assignment pattern holds the listed elements in the order given, and
// a replication inside such a pattern stands for that many copies of the
// element it encloses (LRM 7.10, 10.9.1, Table 7-1).
module Top;
  int listed [$] = '{1, 2, 3};
  int repeated [$] = '{4{7}};
  int uninitialized [$];

  final begin
    if (listed.size() !== 3)
      $fatal(1, "listed.size() was %0d, expected 3", listed.size());
    if (listed[0] !== 1) $fatal(1, "listed[0] was %0d, expected 1", listed[0]);
    if (listed[1] !== 2) $fatal(1, "listed[1] was %0d, expected 2", listed[1]);
    if (listed[2] !== 3) $fatal(1, "listed[2] was %0d, expected 3", listed[2]);

    if (repeated.size() !== 4)
      $fatal(1, "repeated.size() was %0d, expected 4", repeated.size());
    if (repeated[0] !== 7)
      $fatal(1, "repeated[0] was %0d, expected 7", repeated[0]);
    if (repeated[1] !== 7)
      $fatal(1, "repeated[1] was %0d, expected 7", repeated[1]);
    if (repeated[2] !== 7)
      $fatal(1, "repeated[2] was %0d, expected 7", repeated[2]);
    if (repeated[3] !== 7)
      $fatal(1, "repeated[3] was %0d, expected 7", repeated[3]);

    if (uninitialized.size() !== 0)
      $fatal(1, "uninitialized.size() was %0d, expected 0",
             uninitialized.size());
    $display("All checks passed");
  end
endmodule
