// A method of a class a package declares is a task like any other (LRM 8.6),
// so its body may fork (LRM 9.3.2), and a process it forks may disable a block
// it names (LRM 9.6.2) and check an immediate assertion (LRM 16.3). The
// assertion passes, so it reports nothing, and the disabled block leaves the
// rest of it unrun. Each forked process runs to its end before the join
// returns, so the object holds what both wrote by then.
package P;
  class Worker;
    int done = 0;
    int skipped = 0;

    task run();
      fork
        begin : checked
          assert (done == 0);
          done = 1;
        end
        begin : left
          disable left;
          skipped = 1;
        end
      join
    endtask
  endclass
endpackage

module Top;
  initial begin
    automatic P::Worker w = new;
    w.run();
    if (w.done !== 1) $fatal(1, "done was %0d, expected 1", w.done);
    if (w.skipped !== 0) $fatal(1, "skipped was %0d, expected 0", w.skipped);
    $display("All checks passed");
  end
endmodule
