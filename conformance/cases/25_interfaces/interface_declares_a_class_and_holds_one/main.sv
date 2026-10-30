// An interface may declare a class and a variable of it, both being items an
// interface is allowed to contain (LRM 25.3). The interface's own bodies reach
// that handle by its simple name, and a port names the interface's scope rather
// than a point data crosses, so a module reaching the interface through a port
// reaches the same object -- through a subroutine the interface declares
// (LRM 25.7) and by naming the variable itself (LRM 25.10).
interface Bus;
  class Tracker;
    int count = 0;

    function void note();
      count = count + 1;
    endfunction
  endclass

  Tracker tracker = new();

  function int seen();
    return tracker.count;
  endfunction

  task automatic note_twice();
    tracker.note();
    tracker.note();
  endtask
endinterface

module Watcher (Bus b);
  int through_subroutine = -1;
  int through_name = -1;

  initial begin
    b.tracker.note();
    b.note_twice();
    through_subroutine = b.seen();
    through_name = b.tracker.count;
  end
endmodule

module Top;
  int inside_interface = -1;

  Bus bus ();
  Watcher watcher (bus);

  initial begin
    #1;
    inside_interface = bus.seen();
  end

  final begin
    if (watcher.through_subroutine !== 3)
      $fatal(
          1, "through_subroutine was %0d, expected 3",
          watcher.through_subroutine);
    if (watcher.through_name !== 3)
      $fatal(1, "through_name was %0d, expected 3", watcher.through_name);
    if (inside_interface !== 3)
      $fatal(1, "inside_interface was %0d, expected 3", inside_interface);
    $display("All checks passed");
  end
endmodule
