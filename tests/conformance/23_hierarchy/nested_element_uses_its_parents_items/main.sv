// A module declared inside another sees the name space of the module declaring
// it, so it uses what that module declares by the plain name (LRM 23.4), and a
// program nested in a module or an interface shares its variables the same way
// (LRM 24.3). The name reaches the instance of the declaring module that holds
// the nested one: each of two holders is seen by its own nested instances, at
// the width its own specialization gives. An interface nested in a module, a
// module nested two deep, and one instantiated only implicitly, by its own name
// (LRM 23.4), see the same name space. A name of each kind the module declares
// -- a variable, a net, an event, a port, a subroutine, a class handle, a class,
// a named block, an instance, an interface instance -- is reached, read,
// written, driven, forced, waited on and handed on as a port actual. Every
// nested element here is instantiated in the scope that declares it, which LRM
// 23.4 makes the condition for seeing that scope.
interface Bus;
  int data = 0;
endinterface

interface Port;
  int level = 3;
  int seen = 0;
  program Watcher;
    initial #1 seen = level;
  endprogram
endinterface

class Box;
  int v = 41;
endclass

module Holder #(
    parameter int W = 4
) (
    input var int fed
);
  logic [W-1:0] shared = '1;
  wire [W-1:0] mirrored = shared;
  wire [7:0] driven;
  int count = 0;
  int scheduled = 0;
  int forced = 1;
  int comb_seen = 0;
  int program_saw = 0;
  int program_wrote = 0;
  int not_disabled = 0;
  event stirred;
  Box box = new;
  Bus bus ();

  class Kept;
    int v = 9;
  endclass

  initial begin : waiting
    #4;
    not_disabled = 1;
  end

  function automatic int twice(int x);
    return 2 * x;
  endfunction

  task automatic bump();
    count = count + 1;
  endtask

  module Inner;
    int saw_variable = 0;
    int saw_net = 0;
    int saw_port = 0;
    int saw_call = 0;
    int saw_handle = 0;
    int saw_class = 0;
    int saw_sibling = 0;
    int woke = 0;
    int doubled;
    Kept kept = new;

    assign doubled = twice(shared);
    assign driven = 8'd42;
    always_comb comb_seen = shared + 1;

    initial begin
      #1;
      saw_variable = shared;
      saw_net = mirrored;
      saw_port = fed;
      saw_call = twice(shared);
      saw_handle = box.v;
      saw_class = kept.v;
      saw_sibling = sib.v;
      #1;
      bump();
      shared = shared - 1;
      scheduled <= 7;
      force forced = 77;
      disable waiting;
      @(stirred) woke = 1;
    end
  endmodule

  module Sib;
    int v = 8;
  endmodule

  // Never instantiated and without ports, so instantiated once by its name.
  module Implicit;
    int seen = 0;
    initial #1 seen = shared;
  endmodule

  module Mid;
    module Leaf;
      int seen = 0;
      initial #1 seen = shared;
    endmodule
    Leaf leaf ();
  endmodule

  interface Watch;
    int seen = 0;
    initial #1 seen = shared;
  endinterface

  module User (
      Bus b
  );
    initial #3 b.data = 77;
  endmodule

  module Sink (
      input logic [W-1:0] p
  );
    int saw = 0;
    initial #3 saw = p;
  endmodule

  module Deep;
    Sink sink (.p(shared));
    User user (.b(bus));
  endmodule

  Inner u ();
  Sib sib ();
  Mid mid ();
  Deep d ();
  Watch watch ();

  // The run ends once every program has (LRM 24.3), so this one outlasts
  // everything the holder schedules.
  program P;
    initial begin
      #6;
      program_saw = shared;
      program_wrote = 1;
    end
  endprogram

  initial #5 ->stirred;
endmodule

module Top;
  Holder #(4) narrow (.fed(13));
  Holder #(12) wide (.fed(13));
  Holder #(12) other (.fed(14));
  Port port ();

  initial other.shared = 100;

  final begin
    if (narrow.u.saw_variable !== 15)
      $fatal(1, "narrow read %0d, expected 15", narrow.u.saw_variable);
    if (wide.u.saw_variable !== 4095)
      $fatal(1, "wide read %0d, expected 4095", wide.u.saw_variable);
    if (other.u.saw_variable !== 100)
      $fatal(1, "other read %0d, expected its own 100", other.u.saw_variable);
    if (wide.u.saw_net !== 4095)
      $fatal(1, "a net read %0d, expected 4095", wide.u.saw_net);
    if (wide.u.saw_port !== 13 || other.u.saw_port !== 14)
      $fatal(1, "the holders' ports read %0d and %0d", wide.u.saw_port,
             other.u.saw_port);
    if (wide.u.saw_call !== 8190)
      $fatal(1, "a call answered %0d, expected 8190", wide.u.saw_call);
    if (wide.u.doubled !== 8188)
      $fatal(1, "a continuous call answered %0d, expected 8188", wide.u.doubled);
    if (wide.u.saw_handle !== 41)
      $fatal(1, "a handle read %0d, expected 41", wide.u.saw_handle);
    if (wide.u.saw_class !== 9)
      $fatal(1, "an object of the holder's class read %0d", wide.u.saw_class);
    if (wide.u.saw_sibling !== 8)
      $fatal(1, "a sibling instance read %0d, expected 8", wide.u.saw_sibling);
    if (wide.Implicit.seen !== 4095)
      $fatal(1, "the implicit instance read %0d", wide.Implicit.seen);
    if (wide.mid.leaf.seen !== 4095)
      $fatal(1, "a module two deep read %0d", wide.mid.leaf.seen);
    if (wide.watch.seen !== 4095)
      $fatal(1, "a nested interface read %0d, expected 4095", wide.watch.seen);
    if (port.seen !== 3)
      $fatal(1, "a program in an interface read %0d, expected 3", port.seen);
    if (wide.driven !== 8'd42)
      $fatal(1, "the holder's net is driven to %0d", wide.driven);
    if (wide.comb_seen !== 4095)
      $fatal(1, "always_comb left %0d, expected 4095", wide.comb_seen);
    if (wide.forced !== 77)
      $fatal(1, "the force left %0d, expected 77", wide.forced);
    if (wide.not_disabled !== 0)
      $fatal(1, "the holder's named block was not disabled");
    if (wide.bus.data !== 77)
      $fatal(1, "the bus handed down read %0d, expected 77", wide.bus.data);
    if (wide.count !== 1)
      $fatal(1, "the task call left count %0d, expected 1", wide.count);
    if (wide.scheduled !== 7)
      $fatal(1, "the nonblocking write left %0d, expected 7", wide.scheduled);
    if (wide.shared !== 4094)
      $fatal(1, "the write left %0d, expected 4094", wide.shared);
    if (other.shared !== 99)
      $fatal(1, "the write left other at %0d, expected 99", other.shared);
    if (wide.d.sink.saw !== 4094)
      $fatal(1, "a port actual in Deep read %0d, expected 4094", wide.d.sink.saw);
    if (wide.program_saw !== 4094)
      $fatal(1, "the program read %0d, expected 4094", wide.program_saw);
    if (wide.program_wrote !== 1)
      $fatal(1, "the program wrote %0d, expected 1", wide.program_wrote);
    if (wide.u.woke !== 1 || narrow.u.woke !== 1)
      $fatal(1, "a wait on the holder's event did not end");
    $display("All checks passed");
  end
endmodule
