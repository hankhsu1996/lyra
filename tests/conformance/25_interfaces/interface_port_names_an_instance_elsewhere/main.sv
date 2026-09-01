// The actual of an interface port connection names an interface instance
// (LRM 25.3): an element of an array of interfaces, or a hierarchical reference
// that refers to an interface instance and resolves through neither an arrayed
// instance nor a generate block. Each form binds the one instance it names, so
// a module reaches exactly that instance's variables and nothing of a sibling
// element or of another scope's instance.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Holder;
  Bus inner ();
endmodule

module Writer #(
    parameter logic [7:0] Mark = 8'h00
) (
    Bus w
);
  initial #1 begin
    w.addr = Mark;
    w.hits = w.hits + 1;
  end
endmodule

module Top;
  Bus bank[2] ();
  Holder holder ();

  Writer #(8'ha5) first (bank[0]);
  Writer #(8'h5a) second (.w(bank[1]));
  Writer #(8'h3c) nested (holder.inner);

  final begin
    if (bank[0].addr !== 8'ha5)
      $fatal(1, "bank[0].addr was %h, expected a5", bank[0].addr);
    if (bank[1].addr !== 8'h5a)
      $fatal(1, "bank[1].addr was %h, expected 5a", bank[1].addr);
    if (holder.inner.addr !== 8'h3c)
      $fatal(1, "holder.inner.addr was %h, expected 3c", holder.inner.addr);
    if (bank[0].hits !== 1)
      $fatal(1, "bank[0].hits was %0d, expected 1", bank[0].hits);
    if (bank[1].hits !== 1)
      $fatal(1, "bank[1].hits was %0d, expected 1", bank[1].hits);
    if (holder.inner.hits !== 1)
      $fatal(1, "holder.inner.hits was %0d, expected 1", holder.inner.hits);
    $display("All checks passed");
  end
endmodule
