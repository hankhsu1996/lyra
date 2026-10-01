// An event control written @* adds every net and variable its statement reads
// (LRM 9.4.2.2), and that holds wherever the variable is declared. One the
// statement declares with automatic lifetime -- a loop variable, a local at a
// block's head, a foreach index, an array method's iterator -- does not exist
// until the statement is entered and cannot be named from outside it (LRM
// 6.21), so nothing can change it while the control waits; the procedure is
// woken by what else the statement reads. A static one the statement declares
// exists throughout, so a write to it through a hierarchical name wakes the
// procedure. And an automatic the enclosing block declares exists while a
// nested @* waits, so a fork branch writing it wakes that wait.
//
// An array method's iterator is read-only and lives only while its method is
// evaluated (LRM 7.12), so a wait whose own expression introduces one -- an
// event control or a level wait -- is woken by the array it ranges over, and a
// sampled value function over such an expression samples the array (LRM
// 16.5.1).
module Top;
  int a;
  int arr[3];
  int loop_out;
  int head_out;
  int foreach_out;
  int with_out;
  int static_out;
  int nested_out;
  time event_woke = 0;
  time level_woke = 0;
  bit clk;
  int changed_ticks;

  initial repeat (4) #5 clk = !clk;

  always @(posedge clk)
    if ($changed(arr.sum() with (item))) changed_ticks = changed_ticks + 1;

  always @* begin
    loop_out = 0;
    for (int j = 0; j < 3; j = j + 1) loop_out = loop_out + a;
  end

  always @(*) begin
    automatic int k = a;
    head_out = k + 1;
  end

  always @* begin
    foreach_out = 0;
    foreach (arr[i]) foreach_out = foreach_out + arr[i] * a;
  end

  always @* with_out = arr.sum() with (item * a);

  always @* begin : outer
    begin : inner
      static int s;
      static_out = s + a;
    end
  end

  initial begin
    automatic int k = 0;
    fork
      #3 k = 4;
    join_none
    @* nested_out = k;
  end

  initial begin
    fork
      begin @(arr.sum() with (item)); event_woke = $time; end
      begin wait ((arr.sum() with (item)) == 6); level_woke = $time; end
    join_none
  end

  initial begin
    #1;
    arr = '{1, 2, 3};
    a = 2;
    #1;
    outer.inner.s = 5;
  end

  final begin
    if (loop_out !== 6)
      $fatal(1, "a loop variable's statement left %0d, expected 6", loop_out);
    if (head_out !== 3)
      $fatal(1, "a block-head automatic's statement left %0d, expected 3",
             head_out);
    if (foreach_out !== 12)
      $fatal(1, "a foreach statement left %0d, expected 12", foreach_out);
    if (with_out !== 12)
      $fatal(1, "an iterator's statement left %0d, expected 12", with_out);
    if (static_out !== 7)
      $fatal(1, "a static the statement declares left %0d, expected 7",
             static_out);
    if (nested_out !== 4)
      $fatal(1, "an enclosing automatic left %0d, expected 4", nested_out);
    if (event_woke !== 1)
      $fatal(1, "an event control over an iterator ended at %0t, expected 1",
             event_woke);
    if (level_woke !== 1)
      $fatal(1, "a level wait over an iterator ended at %0t, expected 1",
             level_woke);
    if (changed_ticks !== 1)
      $fatal(1, "$changed over an iterator held on %0d ticks, expected 1",
             changed_ticks);
    $display("All checks passed");
  end
endmodule
