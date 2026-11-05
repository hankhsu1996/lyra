// A variable a subroutine declares static is one variable for every call of
// that subroutine (LRM 6.21), and calls run in whatever process made them. An
// event control waits for a value change of an expression whatever declared
// its operands (LRM 9.4.2), so a call waiting on the variable wakes when a
// call in another process writes it.
//
// That holds wherever the subroutine is declared: a module's task, a package's
// task, a static task of a class declared in a package or in the
// compilation-unit scope, and a method of such a class, whose branch forked to
// wait outlives the call that forked it (LRM 9.3.2) and wakes on a write a
// call on another object makes, since one variable serves every object as it
// serves every call.
package waits_pkg;
  time package_task_woke_at = -1;

  task automatic watch_or_bump(bit bump);
    static int n;
    if (bump) n = n + 1;
    else begin
      @(n);
      package_task_woke_at = $time;
    end
  endtask

  class Watcher;
    static time static_task_woke_at = -1;
    time branch_woke_at = -1;

    static task watch_or_bump(bit bump);
      static int n;
      if (bump) n = n + 1;
      else begin
        @(n);
        static_task_woke_at = $time;
      end
    endtask

    task spawn_or_bump(bit bump);
      static int k;
      if (bump) k = k + 1;
      else
        fork
          begin
            @(k);
            branch_woke_at = $time;
          end
        join_none
    endtask
  endclass
endpackage

class UnitWatcher;
  static time woke_at = -1;

  static task watch_or_bump(bit bump);
    static int n;
    if (bump) n = n + 1;
    else begin
      @(n);
      woke_at = $time;
    end
  endtask
endclass

module Top;
  import waits_pkg::*;

  time module_task_woke_at = -1;

  task automatic watch_or_bump(bit bump);
    static int n;
    if (bump) n = n + 1;
    else begin
      @(n);
      module_task_woke_at = $time;
    end
  endtask

  Watcher spawner = new;
  Watcher other = new;

  initial watch_or_bump(0);
  initial #1 watch_or_bump(1);

  initial waits_pkg::watch_or_bump(0);
  initial #2 waits_pkg::watch_or_bump(1);

  initial Watcher::watch_or_bump(0);
  initial #3 Watcher::watch_or_bump(1);

  initial spawner.spawn_or_bump(0);
  initial #4 other.spawn_or_bump(1);

  initial UnitWatcher::watch_or_bump(0);
  initial #5 UnitWatcher::watch_or_bump(1);

  final begin
    if (module_task_woke_at !== 1)
      $fatal(1, "a module task's static woke at %0t, expected 1",
             module_task_woke_at);
    if (waits_pkg::package_task_woke_at !== 2)
      $fatal(1, "a package task's static woke at %0t, expected 2",
             waits_pkg::package_task_woke_at);
    if (Watcher::static_task_woke_at !== 3)
      $fatal(1, "a package class's static task's static woke at %0t, expected 3",
             Watcher::static_task_woke_at);
    if (spawner.branch_woke_at !== 4)
      $fatal(1, "a method's forked branch woke at %0t, expected 4",
             spawner.branch_woke_at);
    if (UnitWatcher::woke_at !== 5)
      $fatal(1, "a compilation-unit class's static task's static woke at %0t, expected 5",
             UnitWatcher::woke_at);
    $display("All checks passed");
  end
endmodule
