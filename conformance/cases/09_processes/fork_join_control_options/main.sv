// The three join keywords differ only in when the forking process resumes:
// join blocks it until every process the fork spawned has terminated, join_any
// until any one of them has, and join_none not at all (LRM 9.3.2, Table 9-1).
// No spawned process starts before the parent blocks or terminates, so what
// tells the three apart is what the parent can already see at the moment it
// resumes -- and the branches it did not wait for go on running afterwards.
module Top;
  int join_time, join_first, join_second;
  int any_time, any_first, any_second, any_leftover;
  int none_time, none_first, none_second, none_leftover;

  int first_done, second_done;

  initial begin
    fork
      #10 first_done = 1;
      #20 second_done = 1;
    join
    join_time = $time;
    join_first = first_done;
    join_second = second_done;

    first_done = 0;
    second_done = 0;
    fork
      #10 first_done = 1;
      #30 second_done = 1;
    join_any
    any_time = $time;
    any_first = first_done;
    any_second = second_done;
    #40;
    any_leftover = second_done;

    first_done = 0;
    second_done = 0;
    fork
      #5 first_done = 1;
      #5 second_done = 1;
    join_none
    none_time = $time;
    none_first = first_done;
    none_second = second_done;
    #10;
    none_leftover = first_done + second_done;
  end

  final begin
    if (join_time !== 20)
      $fatal(1, "join_time was %0d, expected 20", join_time);
    if (join_first !== 1)
      $fatal(1, "join_first was %0d, expected 1", join_first);
    if (join_second !== 1)
      $fatal(1, "join_second was %0d, expected 1", join_second);
    if (any_time !== 30)
      $fatal(1, "any_time was %0d, expected 30", any_time);
    if (any_first !== 1)
      $fatal(1, "any_first was %0d, expected 1", any_first);
    if (any_second !== 0)
      $fatal(1, "any_second was %0d, expected 0", any_second);
    if (any_leftover !== 1)
      $fatal(1, "any_leftover was %0d, expected 1", any_leftover);
    if (none_time !== 70)
      $fatal(1, "none_time was %0d, expected 70", none_time);
    if (none_first !== 0)
      $fatal(1, "none_first was %0d, expected 0", none_first);
    if (none_second !== 0)
      $fatal(1, "none_second was %0d, expected 0", none_second);
    if (none_leftover !== 2)
      $fatal(1, "none_leftover was %0d, expected 2", none_leftover);
    $display("All checks passed");
  end
endmodule
