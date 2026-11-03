// A time scope is a module, program, package, interface or compilation unit
// (LRM 3.14.2.2), and a class is none of them, so a delay written in a class's
// body is read in the time unit and precision of the scope declaring the class
// -- however the body is reached, and however far the class is nested. Here
// the module counts in nanoseconds to the picosecond, so a delay of 1.5 is
// 1.5ns there and in every class it declares, where a coarser precision would
// round it to 2ns; a class of a package counting in tens of nanoseconds waits
// 15ns for the same 1.5, and reads the time in its own unit.
package P;
  timeunit 10ns;
  timeprecision 1ps;

  class InPackage;
    task wait_it();
      #1.5;
    endtask

    function realtime now();
      return $realtime;
    endfunction
  endclass
endpackage

module Top;
  timeunit 1ns;
  timeprecision 1ps;

  class Outer;
    class Inner;
      task wait_it();
        #1.5;
      endtask
    endclass

    task wait_it();
      #1.5;
    endtask

    static task wait_static();
      #1.5;
    endtask

    task wait_in_fork();
      fork
        #1.5;
      join
    endtask
  endclass

  class Param #(int N = 1);
    task wait_it();
      #(1.5 * N);
    endtask
  endclass

  realtime waited[string];
  Outer outer;
  Outer::Inner inner;
  Param #(2) param;
  P::InPackage in_package;
  realtime started;

  initial begin
    outer = new;
    inner = new;
    param = new;
    in_package = new;

    started = $realtime;
    outer.wait_it();
    waited["method"] = $realtime - started;

    started = $realtime;
    Outer::wait_static();
    waited["static method"] = $realtime - started;

    started = $realtime;
    outer.wait_in_fork();
    waited["fork in a method"] = $realtime - started;

    started = $realtime;
    inner.wait_it();
    waited["nested class"] = $realtime - started;

    started = $realtime;
    param.wait_it();
    waited["specialization"] = $realtime - started;

    started = $realtime;
    in_package.wait_it();
    waited["package class"] = $realtime - started;

    if (in_package.now() != $realtime / 10)
      $fatal(1, "a package class read the time as %f at %f ns",
             in_package.now(), $realtime);
  end

  final begin
    if (waited["method"] != 1.5)
      $fatal(1, "a method waited %f", waited["method"]);
    if (waited["static method"] != 1.5)
      $fatal(1, "a static method waited %f", waited["static method"]);
    if (waited["fork in a method"] != 1.5)
      $fatal(1, "a fork in a method waited %f", waited["fork in a method"]);
    if (waited["nested class"] != 1.5)
      $fatal(1, "a nested class waited %f", waited["nested class"]);
    if (waited["specialization"] != 3.0)
      $fatal(1, "a specialization waited %f", waited["specialization"]);
    if (waited["package class"] != 15.0)
      $fatal(1, "a package class waited %f", waited["package class"]);
    $display("All checks passed");
  end
endmodule
