// $timeunit and $timeprecision answer the time unit and precision of the
// design element the call is written in, or of the one it names: an instance,
// $unit, or $root, which answers the simulation time unit for both (LRM
// 20.4.1), as a power of ten (Table 20-2). The simulation time unit is the
// finest precision the design states (LRM 3.14.3). Both are constant system
// function calls, so a parameter may take them and a procedure reads the same
// answers (LRM 11.2.1), wherever in the module the call is written -- a task,
// a class's method, a generate block -- and an instance is named through an
// interface port as through any other name (LRM 25.3), from the calling
// module's own name (LRM 23.8), or from $root (LRM 23.3.1).
timeunit 1us / 1us;
`timescale 1ms / 1us

package Pkg;
  timeunit 10us / 1us;

  function automatic int unit_here();
    return $timeunit;
  endfunction

  function automatic int precision_here();
    return $timeprecision;
  endfunction
endpackage

module Top;
  Bus bus ();
  Middle middle (bus);

  localparam int RootUnit = $timeunit($root);
  localparam int RootPrecision = $timeprecision($root);
  localparam int UnitUnit = $timeunit($unit);
  localparam int UnitPrecision = $timeprecision($unit);
  localparam int OwnUnit = $timeunit;
  localparam int OwnPrecision = $timeprecision;
  localparam int LeafUnit = $timeunit(middle.leaf);
  localparam int LeafPrecision = $timeprecision(middle.leaf);
  localparam int MiddleUnit = $timeunit(Top.middle);
  localparam int MiddlePrecision = $timeprecision(Top.middle);

  task automatic check_from_a_task();
    if ($timeunit(middle.leaf) !== LeafUnit)
      $fatal(1, "middle.leaf read in a task gave %0d", $timeunit(middle.leaf));
  endtask

  class FromAMethod;
    static function void check();
      if ($timeprecision(middle.leaf) !== LeafPrecision)
        $fatal(1, "middle.leaf read in a method gave %0d",
               $timeprecision(middle.leaf));
    endfunction
  endclass

  if (1) begin : nested
    initial
      if ($timeunit(middle.leaf) !== LeafUnit)
        $fatal(1, "middle.leaf read in a generate block gave %0d",
               $timeunit(middle.leaf));
  end

  initial begin
    if (RootUnit !== -7 || RootPrecision !== -7)
      $fatal(1, "$root gave %0d / %0d, expected -7 / -7", RootUnit,
             RootPrecision);
    if (UnitUnit !== -6 || UnitPrecision !== -6)
      $fatal(1, "$unit gave %0d / %0d, expected -6 / -6", UnitUnit,
             UnitPrecision);
    if (OwnUnit !== -3 || OwnPrecision !== -6)
      $fatal(1, "Top gave %0d / %0d, expected -3 / -6", OwnUnit, OwnPrecision);
    if (LeafUnit !== -5 || LeafPrecision !== -7)
      $fatal(1, "middle.leaf gave %0d / %0d, expected -5 / -7", LeafUnit,
             LeafPrecision);
    if (MiddleUnit !== -3 || MiddlePrecision !== -6)
      $fatal(1, "Top.middle gave %0d / %0d, expected -3 / -6", MiddleUnit,
             MiddlePrecision);

    if ($timeunit($unit) !== UnitUnit || $timeprecision($unit) !== UnitPrecision)
      $fatal(1, "$unit read in a procedure gave %0d / %0d", $timeunit($unit),
             $timeprecision($unit));
    if ($timeunit !== OwnUnit || $timeprecision !== OwnPrecision)
      $fatal(1, "Top read in a procedure gave %0d / %0d", $timeunit,
             $timeprecision);
    if ($timeunit(middle.leaf) !== LeafUnit ||
        $timeprecision(middle.leaf) !== LeafPrecision)
      $fatal(1, "middle.leaf read in a procedure gave %0d / %0d",
             $timeunit(middle.leaf), $timeprecision(middle.leaf));
    if ($timeunit(Top.middle) !== MiddleUnit ||
        $timeprecision(Top.middle) !== MiddlePrecision)
      $fatal(1, "Top.middle read in a procedure gave %0d / %0d",
             $timeunit(Top.middle), $timeprecision(Top.middle));

    if (Pkg::unit_here() !== -5 || Pkg::precision_here() !== -6)
      $fatal(1, "Pkg gave %0d / %0d, expected -5 / -6", Pkg::unit_here(),
             Pkg::precision_here());
    check_from_a_task();
    FromAMethod::check();
    #1 $display("All checks passed");
  end
endmodule

module Middle (Bus bus);
  Leaf leaf ();

  initial begin
    if ($timeunit(bus.inner) !== -4 || $timeprecision(bus.inner) !== -6)
      $fatal(1, "bus.inner gave %0d / %0d, expected -4 / -6",
             $timeunit(bus.inner), $timeprecision(bus.inner));
    if ($timeunit($root.Top.middle) !== -3 ||
        $timeprecision($root.Top.middle) !== -6)
      $fatal(1, "$root.Top.middle gave %0d / %0d, expected -3 / -6",
             $timeunit($root.Top.middle), $timeprecision($root.Top.middle));
  end

  if (1) begin : nested
    initial
      if ($timeunit(Middle.leaf) !== -5 || $timeprecision(Middle.leaf) !== -7)
        $fatal(1, "Middle.leaf read in a generate block gave %0d / %0d",
               $timeunit(Middle.leaf), $timeprecision(Middle.leaf));
  end
endmodule

module Leaf;
  timeunit 10us / 100ns;
endmodule

interface Bus;
  Inner inner ();
endinterface

interface Inner;
  timeunit 100us / 1us;
endinterface
