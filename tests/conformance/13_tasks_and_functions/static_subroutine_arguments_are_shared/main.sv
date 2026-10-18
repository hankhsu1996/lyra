// A task or function declared in a module without the automatic keyword is
// static, and so are its formal arguments: there is one variable per argument
// however many calls are in progress, and it keeps its value from one call to
// the next (LRM 13.3.1, 13.3.2, 13.4.2). Two calls of a static task that
// overlap in time therefore share each input, so both see the value the later
// call passed; an output the call does not write hands back what the previous
// call left in it; and a branch a static task or function forks reads the one
// variable when it runs rather than the value its own call passed. An
// automatic task, by contrast, gives each call its own arguments.
module Top;
  int shared_first;
  int shared_second;
  int own_first;
  int own_second;
  int written;
  int left_over;
  int forked_sum;
  int noted_sum;

  task overlap(input int a, output int o);
    #10 o = a;
  endtask

  task automatic overlap_own(input int a, output int o);
    #10 o = a;
  endtask

  task maybe_write(output int o, input bit write);
    if (write) o = 5;
  endtask

  task spawn(int given);
    fork
      #10 forked_sum += given;
    join_none
  endtask

  function void note(int given);
    fork
      #10 noted_sum += given;
    join_none
  endfunction

  // The second call starts a step after the first, so which call is the later
  // one is the program's to say rather than the order the two procedures run
  // in, which nothing fixes (LRM 9.2.1).
  initial overlap(1, shared_first);
  initial #1 overlap(2, shared_second);
  initial overlap_own(1, own_first);
  initial overlap_own(2, own_second);

  initial begin
    maybe_write(written, 1);
    maybe_write(left_over, 0);
    spawn(42);
    spawn(7);
    note(3);
    note(4);
  end

  final begin
    if (shared_first !== 2 || shared_second !== 2)
      $fatal(1, "a static task's overlapping calls returned %0d and %0d, expected 2 and 2",
             shared_first, shared_second);
    if (own_first !== 1 || own_second !== 2)
      $fatal(1, "an automatic task's overlapping calls returned %0d and %0d, expected 1 and 2",
             own_first, own_second);
    if (written !== 5) $fatal(1, "written was %0d, expected 5", written);
    if (left_over !== 5)
      $fatal(1, "left_over was %0d, expected the 5 the previous call left", left_over);
    if (forked_sum !== 14)
      $fatal(1, "the forked branches added up to %0d, expected 7 twice", forked_sum);
    if (noted_sum !== 8)
      $fatal(1, "the function's branches added up to %0d, expected 4 twice", noted_sum);
    $display("All checks passed");
  end
endmodule
