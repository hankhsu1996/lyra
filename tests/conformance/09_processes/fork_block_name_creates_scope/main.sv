// Naming a fork-join block with fork : name creates a new hierarchy scope, so
// a variable declared within it can be referenced hierarchically through that
// name (LRM 9.3.4). A statement label written before the fork keyword is
// equivalent to a block name written after it, and names the same scope (LRM
// 9.3.5). A named block that declares nothing of its own still stands as a
// component of the path to a descendant that does.
module Top;
  initial begin : outer
    fork : worker
      begin : inner
        static int counter = 0;
        #10 counter = 42;
      end
    join

    labelled : fork
      begin : payload
        static int marked = 0;
        #5 marked = 7;
      end
    join
  end

  final begin
    if (Top.outer.worker.inner.counter !== 42)
      $fatal(1, "Top.outer.worker.inner.counter was %0d, expected 42",
             Top.outer.worker.inner.counter);
    if (Top.outer.labelled.payload.marked !== 7)
      $fatal(1, "Top.outer.labelled.payload.marked was %0d, expected 7",
             Top.outer.labelled.payload.marked);
    $display("All checks passed");
  end
endmodule
