// A subroutine an interface declares may take a virtual interface of another
// interface as an argument (LRM 25.9, 13.5), and is called through a virtual
// interface holding an instance of the declaring interface (LRM 25.7). Here
// the chain is two deep: `Outer` relays to `Mid`, which writes `Inner`, and
// the class holding the first handle names only `Outer`. A module holding a
// virtual interface of `Outer` that it never reaches through still compares
// it with the instance it was given and with `null`.
interface Inner;
  int data;
endinterface

interface Mid;
  int relayed;

  function automatic void Put(virtual Inner i, int value);
    i.data  = value;
    relayed = relayed + 1;
  endfunction
endinterface

interface Outer;
  function automatic void Relay(virtual Mid m, virtual Inner i, int value);
    m.Put(i, value);
  endfunction
endinterface

class Walker;
  virtual Outer vo;

  function new(virtual Outer o);
    vo = o;
  endfunction
endclass

module Holder;
  virtual Outer held;
  bit is_given = 1'b0;
  bit is_null = 1'b1;

  task automatic Take(virtual Outer o);
    held = o;
    is_given = (held == o);
    is_null = (held == null);
  endtask
endmodule

module Top;
  Inner inner ();
  Mid mid ();
  Outer outer ();
  Holder holder ();

  initial begin
    Walker w;
    inner.data = -1;
    mid.relayed = 0;
    w = new(outer);
    w.vo.Relay(mid, inner, 42);
    holder.Take(outer);
  end

  final begin
    if (inner.data !== 42) $fatal(1, "inner.data was %0d, expected 42", inner.data);
    if (mid.relayed !== 1) $fatal(1, "mid.relayed was %0d, expected 1", mid.relayed);
    if (holder.is_given !== 1'b1) $fatal(1, "the held handle was not the one given");
    if (holder.is_null !== 1'b0) $fatal(1, "the held handle compared equal to null");
    $display("All checks passed");
  end
endmodule
