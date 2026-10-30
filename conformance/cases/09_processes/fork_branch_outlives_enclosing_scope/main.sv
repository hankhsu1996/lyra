// A fork block leaves processes running once control passes out of it, and
// those processes keep running past the end of the scope that spawned them:
// join_none never waits, and join_any waits only for the first to terminate
// (LRM 9.3.2, Table 9-1). The variables such a process refers to are still the
// enclosing scope's own: an automatic of a fork branch that has already
// terminated, an automatic of a function frame that has already returned, and
// an automatic of a task frame the first branch released, each read back the
// value it held (LRM 6.21). A subroutine's arguments are automatics of its
// frame too, so a branch reads the value each call was given even once a later
// call has taken that frame's place. An output or inout argument is still
// copied back when the task completes (LRM 13.3): what a branch writes to it
// afterwards stays the branch's and never reaches the caller. A method's
// branch reaches the object it was called on after the method has returned and
// the caller has let go of the object, a method that waits reaches it after
// its caller has let go while it waited, and so does a branch a constructor
// forks, before `new` has handed the object over (LRM 8.4, 8.11).
module Top;
  int from_terminated_branch;
  int from_returned_frame;
  int from_released_frame;
  int function_result;
  int from_input[2];
  int from_output;
  int from_inout;
  int from_method;
  int from_waiting_method;
  int from_constructor;
  int output_actual;
  int inout_actual;

  class Counter;
    int member;

    function new(int start);
      member = start;
    endfunction

    task report_later(int add);
      fork
        #10 from_method = member + add;
      join_none
    endtask

    task report_after_waiting(int add);
      #10 from_waiting_method = member + add;
    endtask
  endclass

  class Spawner;
    int member;

    function new(int start);
      member = start;
      fork
        #10 from_constructor = member + 6;
      join_none
    endfunction
  endclass

  task automatic input_reader(int which, int given);
    fork
      #10 from_input[which] = given;
    join_none
  endtask

  task automatic output_writer(output int result);
    result = 1;
    fork
      begin
        #10 result = result + 98;
        from_output = result;
      end
    join_none
  endtask

  task automatic inout_writer(inout int value);
    value = value + 1;
    fork
      begin
        #10 value = value * 10;
        from_inout = value;
      end
    join_none
  endtask

  function automatic int spawn_reader(int base);
    automatic int frame_local = base + 1;
    fork
      #10 from_returned_frame = frame_local;
    join_none
    return base;
  endfunction

  task automatic race_reader(int base);
    automatic int frame_local = base + 2;
    fork
      #1;
      #10 from_released_frame = frame_local;
    join_any
  endtask

  initial begin
    fork
      begin
        automatic int branch_local = 7;
        fork
          #10 from_terminated_branch = branch_local;
        join_none
      end
    join_none
    function_result = spawn_reader(7);
    race_reader(7);
    input_reader(0, 42);
    input_reader(1, 7);
    output_writer(output_actual);
    inout_actual = 4;
    inout_writer(inout_actual);
    begin
      automatic Counter counter = new(30);
      counter.report_later(4);
      counter = null;
      counter = new(1000);
    end
    begin
      automatic Counter waiting = new(50);
      fork
        waiting.report_after_waiting(5);
      join_none
      #1 waiting = null;
      waiting = new(2000);
    end
    begin
      automatic Spawner spawner = new(60);
      automatic Counter after;
      spawner = null;
      after = new(3000);
    end
    if (output_actual !== 1)
      $fatal(1, "output_actual was %0d at return, expected 1", output_actual);
    if (inout_actual !== 5)
      $fatal(1, "inout_actual was %0d at return, expected 5", inout_actual);
    #20;
  end

  final begin
    if (function_result !== 7)
      $fatal(1, "function_result was %0d, expected 7", function_result);
    if (from_terminated_branch !== 7)
      $fatal(1, "from_terminated_branch was %0d, expected 7",
             from_terminated_branch);
    if (from_returned_frame !== 8)
      $fatal(1, "from_returned_frame was %0d, expected 8",
             from_returned_frame);
    if (from_released_frame !== 9)
      $fatal(1, "from_released_frame was %0d, expected 9",
             from_released_frame);
    if (from_input[0] !== 42 || from_input[1] !== 7)
      $fatal(1, "from_input was %0d and %0d, expected 42 and 7",
             from_input[0], from_input[1]);
    if (from_output !== 99)
      $fatal(1, "from_output was %0d, expected 99", from_output);
    if (output_actual !== 1)
      $fatal(1, "output_actual was %0d, expected the 1 copied back", output_actual);
    if (from_inout !== 50)
      $fatal(1, "from_inout was %0d, expected 50", from_inout);
    if (inout_actual !== 5)
      $fatal(1, "inout_actual was %0d, expected the 5 copied back", inout_actual);
    if (from_method !== 34)
      $fatal(1, "from_method was %0d, expected 34", from_method);
    if (from_waiting_method !== 55)
      $fatal(1, "from_waiting_method was %0d, expected 55",
             from_waiting_method);
    if (from_constructor !== 66)
      $fatal(1, "from_constructor was %0d, expected 66", from_constructor);
    $display("All checks passed");
  end
endmodule
