// LRM 25.10 lets a name reach into an interface instance through a port, and
// LRM 23.9 puts a task and a named block on the path inside that instance, as
// LRM 27 puts each generate block the interface elaborates -- so such a name
// ends at a declaration inside one of those scopes as readily as at a member of
// the interface itself, the same way a name into a module instance does: a
// loop's block is selected by the value its index stood at (LRM 27.4), and a
// block a conditional chose by its label (LRM 27.5).
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

  for (genvar i = 0; i < 2; i++) begin : lane
    int data = 40 + i;
  end

  if (1) begin : ctrl
    int mode = 50;
  end
endinterface

module Reader (Bus b);
  int saw_published = 0;
  int saw_task_static = 0;
  int saw_block_static = 0;
  int saw_lane = 0;
  int saw_ctrl = 0;

  initial begin
    #1;
    saw_published = b.published;
    saw_task_static = b.tick.counted;
    saw_block_static = b.watch.kept;
    saw_lane = b.lane[1].data;
    saw_ctrl = b.ctrl.mode;
    b.lane[0].data = 99;
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
    if (r.saw_lane !== 41)
      $fatal(1, "a loop block's variable read %0d, expected 41", r.saw_lane);
    if (r.saw_ctrl !== 50)
      $fatal(1, "a chosen block's variable read %0d, expected 50", r.saw_ctrl);
    if (bus.lane[0].data !== 99)
      $fatal(1, "a write through the port left %0d, expected 99",
             bus.lane[0].data);
    $display("All checks passed");
  end
endmodule
