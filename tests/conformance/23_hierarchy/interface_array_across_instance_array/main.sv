// An actual connected to an array of instances is either distributed among the
// elements or replicated to each of them (LRM 23.3.3.5), and an interface port
// takes both forms: an array of interfaces whose size matches the instance
// array binds element to element in declaration order, while a single interface
// instance binds to every element. Each element therefore holds its own handle,
// so what one element reaches says which instance the connection gave it.
interface Bus;
  logic [7:0] addr;
  int hits;
endinterface

module Writer (
    Bus w
);
  logic [7:0] seen = 8'h3c;

  initial #1 begin
    seen   = w.addr;
    w.hits = w.hits + 1;
  end
endmodule

module Top;
  Bus bank[2] ();
  Bus one ();

  Writer split[2] (bank);
  Writer every[2] (one);

  initial begin
    bank[0].addr = 8'ha5;
    bank[1].addr = 8'h5a;
    one.addr     = 8'hc3;
  end

  final begin
    if (split[0].seen !== 8'ha5)
      $fatal(1, "split[0].seen was %h, expected a5", split[0].seen);
    if (split[1].seen !== 8'h5a)
      $fatal(1, "split[1].seen was %h, expected 5a", split[1].seen);
    if (every[0].seen !== 8'hc3)
      $fatal(1, "every[0].seen was %h, expected c3", every[0].seen);
    if (every[1].seen !== 8'hc3)
      $fatal(1, "every[1].seen was %h, expected c3", every[1].seen);
    if (bank[0].hits !== 1)
      $fatal(1, "bank[0].hits was %0d, expected 1", bank[0].hits);
    if (bank[1].hits !== 1)
      $fatal(1, "bank[1].hits was %0d, expected 1", bank[1].hits);
    if (one.hits !== 2)
      $fatal(1, "one.hits was %0d, expected 2", one.hits);
    $display("All checks passed");
  end
endmodule
