// $exit waits for all program blocks to complete and then makes an implicit
// call to $finish (LRM 20.2, 24.7). With no program blocks it ends simulation
// where it is reached: the statement after it does not run, and a final
// procedure runs because simulation ended through an implicit $finish
// (LRM 9.2.3). The calling process has waited and holds an automatic variable
// of its own at the call, which changes none of this.
module Top;
  int reached_after_exit;
  int completed;

  initial begin
    automatic string note = "exiting";
    completed = 3;
    reached_after_exit = 7;
    #1;
    completed = note.len();
    $exit;
    reached_after_exit = 1;
  end

  final begin
    if (completed !== 7)
      $fatal(1, "completed was %0d, expected 7", completed);
    if (reached_after_exit !== 7)
      $fatal(1, "reached_after_exit was %0d, expected 7", reached_after_exit);
    $display("All checks passed");
  end
endmodule
