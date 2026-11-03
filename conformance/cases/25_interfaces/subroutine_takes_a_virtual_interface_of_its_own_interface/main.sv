// A virtual interface type names an interface together with its parameters
// (LRM 25.9), so an interface may declare a subroutine taking one of its own
// interface, and two interfaces may each take one of the other. Each instance
// is assignable to the type naming its interface wherever that type is
// written: in a module, in the interface itself, or in the other one. A
// localparam of the interface may name that type too (LRM 6.20.3), and it is
// then the type the module writes with the same parameters.
interface Node;
  int value;

  function automatic void CopyFrom(virtual Node other);
    value = other.value;
  endfunction
endinterface

interface Feeder;
  int value;

  function automatic void Feed(virtual Puller into);
    into.value = value;
  endfunction
endinterface

interface Puller;
  int value;

  function automatic void Pull(virtual Feeder from, virtual Puller self);
    from.Feed(self);
  endfunction
endinterface

interface Ring #(parameter int W = 1);
  localparam type Self = virtual Ring #(W);
  logic [W-1:0] value;

  function automatic void CopyFrom(Self other);
    value = other.value;
  endfunction
endinterface

module Top;
  Node a ();
  Node b ();
  Feeder feeder ();
  Puller puller ();
  Ring #(2) first ();
  Ring #(2) second ();
  virtual Node held;
  virtual Ring #(2) ring;

  initial begin
    a.value = 7;
    held = a;
    b.CopyFrom(held);
    feeder.value = 5;
    puller.Pull(feeder, puller);
    first.value = 2'b10;
    ring = first;
    second.CopyFrom(ring);
  end

  final begin
    if (b.value !== 7) $fatal(1, "b.value was %0d, expected 7", b.value);
    if (puller.value !== 5)
      $fatal(1, "puller.value was %0d, expected 5", puller.value);
    if (second.value !== 2'b10)
      $fatal(1, "second.value was %b, expected 10", second.value);
    $display("All checks passed");
  end
endmodule
