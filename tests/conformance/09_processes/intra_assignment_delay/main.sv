// A non-blocking assignment carrying an intra-assignment delay reads its
// right-hand side where the statement is reached and schedules the update into
// the NBA region of the slot that delay names, without suspending the procedure
// (LRM 9.4.5, 4.4.2.4, 10.4.2). Several such updates to one variable stay live
// at once, and each lands at its own time. The right-hand side and the
// left-hand side's index are evaluated before the delay.
module Top;
  int source = 1;
  int target = 0;
  int seen_before_deadline = 9;
  int stepped = 9;
  int stepped_at_four = 9;
  int stepped_at_eight = 9;

  initial begin
    target <= #10 source;
    // The procedure did not block, so it is still at time zero here and the
    // update has not been made.
    seen_before_deadline = target;
  end

  // LRM 10.4.2: scheduling one update does not cancel another already pending
  // on the same variable, so all three land, each at its own time.
  initial begin
    stepped <= #4 1;
    stepped <= #8 2;
    stepped <= #12 3;
  end

  initial begin
    #6 stepped_at_four = stepped;
    #4 stepped_at_eight = stepped;
  end

  // LRM 4.6: updates are performed in the order they were placed, so one placed
  // at time zero for time 20 lands before one placed at time 20 itself.
  int ordered = 0;
  initial begin
    ordered <= #20 1;
    #20 ordered <= 2;
  end

  // Rewriting the operand while the delays are under way reaches no update,
  // because every right-hand side was read at time zero.
  initial #5 source = 99;

  // LRM 9.4.5 Table 9-3: the right-hand side is read into a temporary, and the
  // delay is taken after it, so the right-hand side is evaluated first.
  int evaluated [$];
  int read_first = 0;
  function automatic int right_side();
    evaluated.push_back(1);
    return 5;
  endfunction
  function automatic int delay_amount();
    evaluated.push_back(2);
    return 3;
  endfunction
  initial read_first <= #(delay_amount()) right_side();

  // The same order where the delay writes what the statement read: the update
  // stores the right-hand side and lands at the index as they were before the
  // delay was evaluated.
  int changed_by_delay = 5;
  int index_before_delay = 1;
  int landed [4] = '{0, 0, 0, 0};
  function automatic int delay_that_writes();
    changed_by_delay = 99;
    index_before_delay = 3;
    return 2;
  endfunction
  initial landed[index_before_delay] <= #(delay_that_writes()) changed_by_delay;

  // The same where what the statement read is a string or a value too wide for
  // a machine word, a static local, or a property of an object the delay
  // reaches through another handle, by a nonblocking and by a blocking
  // assignment. A delay of a nonblocking assignment may not name an automatic
  // variable (LRM 13.3.2), so one can be written only by a blocking one.
  string text_source = "before";
  string text_landed = "";
  string text_blocking = "";
  logic [3999:0] wide_source = '1;
  logic [3999:0] wide_landed = '0;
  function automatic int delay_that_writes_text();
    text_source = "after";
    return 2;
  endfunction
  function automatic int delay_that_writes_wide();
    wide_source = '0;
    return 2;
  endfunction
  initial text_landed <= #(delay_that_writes_text()) text_source;
  initial wide_landed <= #(delay_that_writes_wide()) wide_source;

  string static_landed = "";
  function automatic int delay_that_writes_string(ref string s);
    s = "after";
    return 2;
  endfunction
  initial begin
    static string kept = "before";
    static_landed <= #(delay_that_writes_string(kept)) kept;
  end

  class Holder;
    string text;
  endclass
  Holder shared_holder;
  string property_landed = "";
  function automatic int delay_that_writes_property();
    shared_holder.text = "after";
    return 2;
  endfunction
  initial begin
    automatic Holder held = new;
    held.text = "before";
    shared_holder = held;
    property_landed <= #(delay_that_writes_property()) held.text;
  end
  initial begin
    automatic string from = "blocking before";
    text_blocking = #(clobber(from)) from;
  end
  function automatic int clobber(ref string s);
    s = "blocking after";
    return 1;
  endfunction

  final begin
    if (seen_before_deadline !== 0)
      $fatal(
          1, "the procedure read %0d without blocking, expected the un-updated 0",
          seen_before_deadline);
    if (target !== 1)
      $fatal(1, "the scheduled update stored %0d, expected the operand read at time zero", target);
    if (stepped_at_four !== 1)
      $fatal(1, "at time 6 the variable held %0d, expected 1", stepped_at_four);
    if (stepped_at_eight !== 2)
      $fatal(1, "at time 10 the variable held %0d, expected 2", stepped_at_eight);
    if (stepped !== 3)
      $fatal(1, "the last update left %0d, expected 3", stepped);
    if (ordered !== 2)
      $fatal(1, "an update placed earlier for one slot landed after one placed in it: %0d", ordered);
    if (evaluated.size() !== 2 || evaluated[0] !== 1 || evaluated[1] !== 2)
      $fatal(1, "the right-hand side and the delay were evaluated in the order %p, expected '{1, 2}",
             evaluated);
    if (read_first !== 5)
      $fatal(1, "the delayed update stored %0d, expected 5", read_first);
    if (landed[1] !== 5 || landed[3] !== 0)
      $fatal(1, "a delay that wrote what the statement read reached the update: %p", landed);
    if (text_landed != "before")
      $fatal(1, "a delay that wrote a string reached the update: \"%s\"", text_landed);
    if (wide_landed !== {4000{1'b1}})
      $fatal(1, "a delay that wrote a wide value reached the update: %h", wide_landed[7:0]);
    if (static_landed != "before")
      $fatal(1, "a delay that wrote a static local reached the update: \"%s\"",
             static_landed);
    if (property_landed != "before")
      $fatal(1, "a delay that wrote a property reached the update: \"%s\"",
             property_landed);
    if (text_blocking != "blocking before")
      $fatal(1, "a delay that wrote a string reached the blocking assignment: \"%s\"",
             text_blocking);
    $display("All checks passed");
  end
endmodule
