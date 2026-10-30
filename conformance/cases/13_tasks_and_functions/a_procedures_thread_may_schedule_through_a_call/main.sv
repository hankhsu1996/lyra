// @reports-none: error
//
// A function may schedule what cannot become active until it returns -- a
// nonblocking assignment in every form, a nonblocking event trigger, a
// fork-join_none -- when the thread calling it was created by an initial or
// always procedure, or a fork of one (LRM 13.4.4). A branch the function forks
// is a process of its own and may wait and call a task. A call from any other
// thread is no error while it schedules nothing. An automatic variable
// declared in an initial procedure is initialized by that procedure (LRM 6.21),
// so a call in its initializer is the procedure's.
module Top;
  bit clk;
  bit go;
  event e, never;
  int from_initial, from_always, from_fork, from_initializer, delayed, branch_waited, unused, y;
  bit triggered;

  function void schedule_from_initial();
    from_initial <= 1;
  endfunction

  function void schedule_from_always();
    from_always <= 1;
  endfunction

  function void schedule_from_fork();
    from_fork <= 1;
  endfunction

  function void schedule_every_form();
    delayed <= #1 1;
    unused <= @(never) 1;
    ->> e;
    fork
      begin
        #2;
        wait_a_step();
        branch_waited = 1;
      end
    join_none
  endfunction

  task wait_a_step();
    #1;
  endtask

  function int schedule_from_initializer();
    from_initializer <= 1;
    return 3;
  endfunction

  function int schedule_when(bit when);
    if (when) unused <= 2;
    return 0;
  endfunction

  assign y = schedule_when(go);

  always @(e) triggered = 1;

  always @(posedge clk) schedule_from_always();

  initial begin
    automatic int returned = 5;
    from_initial = 5;
    from_always = 5;
    from_fork = 5;
    delayed = 5;
    branch_waited = 5;
    unused = 5;
    from_initializer = 5;
    #1;
    begin
      automatic int initialized = schedule_from_initializer();
      returned = initialized;
    end
    schedule_from_initial();
    schedule_every_form();
    fork
      schedule_from_fork();
    join
    clk = 1;
    #5;
    if (from_initial !== 1) $fatal(1, "from_initial was %0d", from_initial);
    if (from_always !== 1) $fatal(1, "from_always was %0d", from_always);
    if (from_fork !== 1) $fatal(1, "from_fork was %0d", from_fork);
    if (delayed !== 1) $fatal(1, "delayed was %0d", delayed);
    if (!triggered) $fatal(1, "the nonblocking trigger never landed");
    if (branch_waited !== 1) $fatal(1, "branch_waited was %0d", branch_waited);
    if (unused !== 5) $fatal(1, "unused was %0d", unused);
    if (from_initializer !== 1)
      $fatal(1, "from_initializer was %0d", from_initializer);
    if (returned !== 3) $fatal(1, "returned was %0d", returned);
    $display("All checks passed");
  end
endmodule
