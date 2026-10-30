// An interface port is connected to any interface instance a hierarchical name
// reaches (LRM 25.3, 23.6), and a generic one takes whichever interface that
// is (LRM 25.3.3). So a module may carry an interface that itself carries an
// interface standing inside that module: what the module is depends on the
// first, the first on the second, and the second -- declared by the module
// (LRM 23.4), or writing a name that lands in it (LRM 23.8) -- on the module.
interface Carrier (interface inner);
endinterface

interface Reader;
  int seen;
  initial seen = Holder.tag;
endinterface

module Holder (Carrier c);
  int tag = 7;
  interface Declared;
    int v = 3;
  endinterface
  Declared declared ();
  Reader reader ();
endmodule

module Top;
  Carrier by_declaration (.inner(first.declared));
  Holder first (.c(by_declaration));

  Carrier by_name (.inner(second.reader));
  Holder second (.c(by_name));

  final begin
    if (first.declared.v !== 3) $fatal(1, "first.declared.v was %0d", first.declared.v);
    if (second.reader.seen !== 7) $fatal(1, "second.reader.seen was %0d", second.reader.seen);
    $display("All checks passed");
  end
endmodule
