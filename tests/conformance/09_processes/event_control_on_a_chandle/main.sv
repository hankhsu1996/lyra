// An event control on a chandle variable waits for a write to it that is not
// equal to its previous value (LRM 9.4.2). Writing the pointer it already
// holds is no event; writing a different one is, and so is writing null over
// one that names something. LRM 6.14 lists an event expression among the uses
// a chandle may not have, while 9.4.2 states what waiting on one means; the
// front end admits the form, and this is the meaning 9.4.2 gives it. A chandle
// a task declares is waited on the same way, written by a branch it forks.
module Top;
  import "DPI-C" function chandle allocate_token();
  import "DPI-C" function void release_token(input chandle token);

  chandle first;
  chandle second;
  chandle watched;
  time to_other_woke = 0;
  time to_null_woke = 0;
  time local_woke = 0;

  task automatic watch_a_local();
    chandle mine;
    fork
      #20 mine = second;
    join_none
    @(mine);
    local_woke = $time;
  endtask

  initial watch_a_local();

  initial begin
    first = allocate_token();
    second = allocate_token();
    watched = first;
    begin
      fork
        begin
          @(watched);
          to_other_woke = $time;
        end
      join_none
      #5 watched = first;
      #5 watched = second;
    end
    fork
      begin
        @(watched);
        to_null_woke = $time;
      end
    join_none
    #5 watched = null;
    #1 release_token(first);
    #10 release_token(second);
  end

  final begin
    if (to_other_woke !== 10) $fatal(1, "the wait for a different pointer ended at %0t, expected 10", to_other_woke);
    if (to_null_woke !== 15) $fatal(1, "the wait for null ended at %0t, expected 15", to_null_woke);
    if (local_woke !== 20) $fatal(1, "the wait on a task's chandle ended at %0t, expected 20", local_woke);
    $display("All checks passed");
  end
endmodule
