// An interface referenced as a port is a bundled collection of variables, and
// when it is, the variables in it are accessed by reference (LRM 25.3.2). A
// write through the port is therefore a write to the one variable of the
// interface instance the connection named, with no copy standing between them:
// every module given that instance reads the written value and a procedure
// sensitive to it wakes, while a module given a different instance of the same
// interface sees nothing of it, even though both modules are the same design
// element. The connection is made either by position or by name, and the port's
// name inside the module is independent of the interface instance's name
// outside it.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Writer (
    Bus w
);
  initial #1 begin
    w.addr = 8'ha5;
    w.hits = w.hits + 1;
  end
endmodule

module Reader (
    Bus r
);
  logic [7:0] seen = 8'h3c;
  logic [7:0] mirror = 8'h3c;

  always @(r.addr) mirror = r.addr;

  initial #2 begin
    seen   = r.addr;
    r.hits = r.hits + 10;
  end
endmodule

module Top;
  Bus shared ();
  Bus other ();

  Writer writer (.w(shared));
  Reader reader (.r(shared));
  Writer lonely (other);

  final begin
    if (shared.addr !== 8'ha5)
      $fatal(1, "shared.addr was %h, expected a5", shared.addr);
    if (reader.seen !== 8'ha5)
      $fatal(1, "reader.seen was %h, expected a5", reader.seen);
    if (reader.mirror !== 8'ha5)
      $fatal(1, "reader.mirror was %h, expected a5", reader.mirror);
    if (shared.hits !== 11)
      $fatal(1, "shared.hits was %0d, expected 11", shared.hits);
    if (other.addr !== 8'ha5)
      $fatal(1, "other.addr was %h, expected a5", other.addr);
    if (other.hits !== 1)
      $fatal(1, "other.hits was %0d, expected 1", other.hits);
    $display("All checks passed");
  end
endmodule
