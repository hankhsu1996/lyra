// Table 8-1 makes an unreferenced object eligible for collection and leaves a
// referenced one alone (LRM 8.4), and a variable of class type holding a handle
// is what references the object. An automatic subroutine's variables are
// deallocated at the end of the invocation (LRM 13.3.2), and a suspension
// inside one is not the end of it, so nothing a wait does unreferences the
// object: the variable goes on naming it, its properties read back what was
// written before the wait, and a second handle taken before the wait still
// compares equal to it after. That holds for a process's own automatic
// variable, for a subroutine's formal, for a local of a subroutine the enclosing
// scope declared automatic, and for a local of a class method -- which is
// automatic whatever encloses it (LRM 8.6), so this is the ordinary case rather
// than a declared exception.
class Cell;
  int value;
endclass

class Worker;
  int seen;
  bit same_object;

  task run();
    Cell own = new();
    Cell taken_before;
    own.value = 40;
    taken_before = own;
    #1;
    own.value = own.value + 2;
    seen = own.value;
    same_object = (own === taken_before);
  endtask
endclass

module Top;
  int in_process;
  bit in_process_same;
  int through_formal;
  bit through_formal_same;
  int in_automatic_task;
  bit in_automatic_task_same;
  int in_method;
  bit in_method_same;

  Cell passed;

  task automatic hold_a_formal(Cell held, Cell taken_before);
    #1;
    held.value = held.value + 2;
    through_formal = held.value;
    through_formal_same = (held === taken_before);
  endtask

  task automatic hold_a_local();
    Cell mine = new();
    Cell taken_before;
    mine.value = 20;
    taken_before = mine;
    #1;
    mine.value = mine.value + 2;
    in_automatic_task = mine.value;
    in_automatic_task_same = (mine === taken_before);
  endtask

  initial begin
    in_process = -1;
    through_formal = -1;
    in_automatic_task = -1;
    in_method = -1;
  end

  initial begin
    automatic Cell own = new();
    automatic Cell taken_before;
    own.value = 10;
    taken_before = own;
    #1;
    own.value = own.value + 2;
    in_process = own.value;
    in_process_same = (own === taken_before);
  end

  initial begin
    passed = new();
    passed.value = 30;
    hold_a_formal(passed, passed);
  end

  initial hold_a_local();

  initial begin
    Worker worker = new();
    worker.run();
    in_method = worker.seen;
    in_method_same = worker.same_object;
  end

  final begin
    if (in_process !== 12)
      $fatal(1, "in_process was %0d, expected 12", in_process);
    if (in_process_same !== 1'b1)
      $fatal(1, "a process's own handle compared to its alias as %b, expected 1",
             in_process_same);
    if (through_formal !== 32)
      $fatal(1, "through_formal was %0d, expected 32", through_formal);
    if (through_formal_same !== 1'b1)
      $fatal(1, "a formal compared to the caller's handle as %b, expected 1",
             through_formal_same);
    if (in_automatic_task !== 22)
      $fatal(1, "in_automatic_task was %0d, expected 22", in_automatic_task);
    if (in_automatic_task_same !== 1'b1)
      $fatal(1, "a task's own handle compared to its alias as %b, expected 1",
             in_automatic_task_same);
    if (in_method !== 42)
      $fatal(1, "in_method was %0d, expected 42", in_method);
    if (in_method_same !== 1'b1)
      $fatal(1, "a method's own handle compared to its alias as %b, expected 1",
             in_method_same);
    $display("All checks passed");
  end
endmodule
