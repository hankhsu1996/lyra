// A subroutine an interface declares acts on the interface instance the caller
// reached it through (LRM 25.7), which is what lets a module drive a bus
// without naming any of its wires: the function acts on that instance's own
// members, and the module reaches it by naming it on the port. A modport
// `import` states which of them a restricted view offers, while a port naming no
// modport reaches every subroutine the interface declares, and a scope that owns
// the instance enables one by hierarchical name (LRM 25.10). Because the call
// acts on the instance it was reached through, two modules bound to different
// instances of one interface leave different storage changed.
interface Bus;
  logic [7:0] data;
  int         reads;

  function automatic void Write(input logic [7:0] value);
    data = value;
  endfunction

  function automatic logic [7:0] Read;
    reads = reads + 1;
    return data;
  endfunction

  modport writer(import Write);
  modport reader(import Read);
endinterface

module Writer (
    Bus.writer w
);
  initial #1 w.Write(8'h5a);
endmodule

module Reader (
    Bus.reader r
);
  logic [7:0] got = 8'h00;
  initial #2 got = r.Read();
endmodule

// No modport, so every subroutine the interface declares is reachable.
module Both (
    Bus b
);
  initial #3 b.Write(b.Read() + 8'd1);
endmodule

module Top;
  Bus first ();
  Bus second ();

  Writer writer (first.writer);
  Reader reader (first.reader);
  Both both (first);

  Writer lonely (second.writer);
  Reader onlooker (second.reader);

  logic [7:0] echoed = 8'h00;

  // The scope that owns the instance enables the same subroutines on it by
  // hierarchical name, with no port between.
  initial #6 begin
    first.Write(8'hff);
    echoed = first.Read();
  end

  final begin
    if (first.data !== 8'hff)
      $fatal(1, "first.data was %h, expected ff", first.data);
    if (second.data !== 8'h5a)
      $fatal(1, "second.data was %h, expected 5a", second.data);
    if (echoed !== 8'hff) $fatal(1, "echoed was %h, expected ff", echoed);
    if (reader.got !== 8'h5a)
      $fatal(1, "reader.got was %h, expected 5a", reader.got);
    if (onlooker.got !== 8'h5a)
      $fatal(1, "onlooker.got was %h, expected 5a", onlooker.got);
    // Three calls reached `first` and one reached `second`, so a count that
    // agreed across them would mean one instance's storage served both.
    if (first.reads !== 3)
      $fatal(1, "first.reads was %0d, expected 3", first.reads);
    if (second.reads !== 1)
      $fatal(1, "second.reads was %0d, expected 1", second.reads);
    $display("All checks passed");
  end
endmodule
