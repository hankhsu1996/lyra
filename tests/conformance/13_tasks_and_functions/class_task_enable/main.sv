// A class method may be a task, and enabling one through an object behaves as
// any other task enable does: it may consume time, and the enabling statement
// resumes only when the task has completed, so an output formal reaches its
// actual then and not before. What a class adds is lifetime. A method declared
// in a class is always automatic and may not be given a static lifetime, so
// every activation allocates its own formals and locals; two activations of one
// task running at once therefore cannot see each other's, while the object they
// were enabled through is one object both reach across the suspension. Being a
// task changes nothing about how the method is selected: a virtual task reached
// through a base class handle runs the derived override, a static task has no
// receiver and answers to the class name as readily as to a handle, and an
// abstract class may declare one as a pure virtual prototype for a subclass to
// implement (LRM 8.6, 8.10, 8.20, 8.21, 13.3, 13.3.1, 13.5).
module Top;
  timeunit 1ns;
  timeprecision 1ps;

  class Meter;
    int total;

    task accumulate(input int start, input int step, output int result);
      int running;
      running = start;
      #5;
      running = running + step;
      total = total + step;
      result = running;
    endtask

    virtual task tag(output int code);
      #1;
      code = 10;
    endtask

    static task scale(input int value, output int scaled);
      #2;
      scaled = value * 3;
    endtask
  endclass

  class Gauge extends Meter;
    virtual task tag(output int code);
      #1;
      code = 20;
    endtask
  endclass

  virtual class Stage;
    pure virtual task settle(input int mark, output int echoed);
  endclass

  class Delayed extends Stage;
    virtual task settle(input int mark, output int echoed);
      #3;
      echoed = mark * 2;
    endtask
  endclass

  Meter m;
  Gauge g;
  Meter base_handle;
  Delayed d;
  Stage stage;

  int single_result;
  int enable_time;
  int mid_result;
  int mid_time;

  int first_result;
  int second_result;
  int concurrent_time;
  int running_total;

  int base_tag;
  int derived_tag;
  int scaled_by_name;
  int scaled_by_handle;
  int contract_echo;

  initial begin
    single_result = -1;
    first_result = -1;
    second_result = -1;
    base_tag = -1;
    derived_tag = -1;
    scaled_by_name = -1;
    scaled_by_handle = -1;
    contract_echo = -1;
    enable_time = -1;
    concurrent_time = -1;
    running_total = -1;

    m = new;
    m.accumulate(100, 1, single_result);
    enable_time = $time;

    fork
      m.accumulate(200, 2, first_result);
      m.accumulate(300, 3, second_result);
    join
    concurrent_time = $time;
    running_total = m.total;

    g = new;
    base_handle = g;
    base_handle.tag(derived_tag);
    m.tag(base_tag);

    Meter::scale(4, scaled_by_name);
    m.scale(5, scaled_by_handle);

    d = new;
    stage = d;
    stage.settle(6, contract_echo);
  end

  initial begin
    #2;
    mid_result = single_result;
    mid_time = $time;
  end

  final begin
    if (mid_time !== 2) $fatal(1, "mid_time was %0d, expected 2", mid_time);
    if (mid_result !== -1)
      $fatal(1, "mid_result was %0d, expected -1", mid_result);
    if (enable_time !== 5)
      $fatal(1, "enable_time was %0d, expected 5", enable_time);
    if (single_result !== 101)
      $fatal(1, "single_result was %0d, expected 101", single_result);
    if (first_result !== 202)
      $fatal(1, "first_result was %0d, expected 202", first_result);
    if (second_result !== 303)
      $fatal(1, "second_result was %0d, expected 303", second_result);
    if (concurrent_time !== 10)
      $fatal(1, "concurrent_time was %0d, expected 10", concurrent_time);
    if (running_total !== 6)
      $fatal(1, "running_total was %0d, expected 6", running_total);
    if (derived_tag !== 20)
      $fatal(1, "derived_tag was %0d, expected 20", derived_tag);
    if (base_tag !== 10) $fatal(1, "base_tag was %0d, expected 10", base_tag);
    if (scaled_by_name !== 12)
      $fatal(1, "scaled_by_name was %0d, expected 12", scaled_by_name);
    if (scaled_by_handle !== 15)
      $fatal(1, "scaled_by_handle was %0d, expected 15", scaled_by_handle);
    if (contract_echo !== 12)
      $fatal(1, "contract_echo was %0d, expected 12", contract_echo);
    $display("All checks passed");
  end
endmodule
