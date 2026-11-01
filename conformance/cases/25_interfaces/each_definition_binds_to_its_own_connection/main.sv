// Which module defines a subroutine an interface leaves to a connected module
// is a fact of each interface instance, not of the interface (LRM 25.7). Two
// instances of one interface are defined by two instances of one module, each
// running on its own state; two subroutines of one interface are defined by two
// different modules; one module defines the same subroutine for both of its
// ports, with a different body for each; a parameterized interface and module
// work as each of their specializations states; and a definition stands apart
// from a subroutine whose escaped name spells the same text (LRM 5.6.1).
interface Bus;
  extern function int Read();
  extern function void Write(input int value);
  modport reader(export Read);
  modport writer(export Write);
endinterface

module Store (
    Bus.reader r
);
  int held;

  function int r.Read();
    return held;
  endfunction
endmodule

// Writes into a `Store` by hierarchical name, so the two definitions of one
// interface act on one place.
module Loader (
    Bus.writer w
);
  int writes = 0;

  function void w.Write(input int value);
    writes = writes + 1;
    $root.Top.store_a.held = value;
  endfunction
endmodule

interface Port;
  extern function int Id();
  modport provider(export Id);
endinterface

module Twin (
    Port.provider left,
    Port.provider right
);
  int base = 100;

  function int left.Id();
    return base + 1;
  endfunction

  function int right.Id();
    return base + 2;
  endfunction

  function int \right.Id ();
    return base + 3;
  endfunction
endmodule

interface Sized #(
    parameter int WIDTH = 8
);
  extern function logic [WIDTH-1:0] Max();
  modport provider(export Max);
endinterface

module Filler #(
    parameter int WIDTH = 8
) (
    Sized.provider s
);
  function logic [WIDTH-1:0] s.Max();
    return '1;
  endfunction
endmodule

module Top;
  Sized #(.WIDTH(4)) narrow ();
  Sized #(.WIDTH(12)) wide ();
  Filler #(.WIDTH(4)) fill_narrow (narrow.provider);
  Filler #(.WIDTH(12)) fill_wide (wide.provider);

  Bus bus_a ();
  Bus bus_b ();
  Store store_a (bus_a.reader);
  Store store_b (bus_b.reader);
  Loader loader (bus_a.writer);
  Loader idle (bus_b.writer);

  Port p ();
  Port q ();
  Twin twin (p.provider, q.provider);

  initial begin
    store_b.held = 5;
    bus_a.Write(42);
    twin.base = 200;
  end

  final begin
    if (bus_a.Read() !== 42)
      $fatal(1, "bus_a.Read() was %0d, expected 42", bus_a.Read());
    if (bus_b.Read() !== 5)
      $fatal(1, "bus_b.Read() was %0d, expected 5", bus_b.Read());
    if (loader.writes !== 1)
      $fatal(1, "loader.writes was %0d, expected 1", loader.writes);
    if (idle.writes !== 0)
      $fatal(1, "idle.writes was %0d, expected 0", idle.writes);
    if (p.Id() !== 201) $fatal(1, "p.Id() was %0d, expected 201", p.Id());
    if (q.Id() !== 202) $fatal(1, "q.Id() was %0d, expected 202", q.Id());
    if (twin.\right.Id () !== 203)
      $fatal(1, "the escaped name was %0d, expected 203", twin.\right.Id ());
    if (narrow.Max() !== 4'hf)
      $fatal(1, "narrow.Max() was %0h, expected f", narrow.Max());
    if (wide.Max() !== 12'hfff)
      $fatal(1, "wide.Max() was %0h, expected fff", wide.Max());
    $display("All checks passed");
  end
endmodule
