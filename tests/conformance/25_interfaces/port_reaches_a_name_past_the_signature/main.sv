// LRM 25.10 lets a name reach into an interface instance through a port, and
// LRM 23.9 puts a task and a named block on the path inside that instance -- so
// such a name ends at a declaration the interface never published as readily as
// at a member it did. What the interface promised decides how each step is
// reached and never how far the name may go: the promised step is checked where
// this module compiles, and a step past it is answered by the instance while
// the design elaborates, exactly as the same step into a module instance is.
interface Bus;
  int published = 11;

  task static tick();
    int counted = 22;
    #10;
  endtask

  initial tick();

  initial begin : watch
    int kept = 33;
    #10;
  end
endinterface

module Reader (Bus b);
  int saw_published = 0;
  int saw_task_static = 0;
  int saw_block_static = 0;

  initial begin
    #1;
    saw_published   = b.published;
    saw_task_static = b.tick.counted;
    saw_block_static = b.watch.kept;
  end
endmodule

module Top;
  Bus    bus ();
  Reader r (.b(bus));

  final begin
    if (r.saw_published !== 11)
      $fatal(1, "a published member read %0d, expected 11", r.saw_published);
    if (r.saw_task_static !== 22)
      $fatal(1, "a task's static read %0d, expected 22", r.saw_task_static);
    if (r.saw_block_static !== 33)
      $fatal(1, "a named block's static read %0d, expected 33", r.saw_block_static);
    $display("All checks passed");
  end
endmodule
