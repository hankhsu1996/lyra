// A class may hold a `chandle` (LRM 6.14) and a virtual interface (LRM 25.9)
// as properties, and the constructor may set both from its arguments. As for
// any property, each holds its type's uninitialized value, `null` for both, by
// the time the constructor's body runs, and a class declaring no constructor
// leaves them so (LRM 8.7). An argument left to its default is `null` where
// the default says so, and the constructor stores that like any other value.
interface Port;
  int tag;
endinterface

module Top;
  import "DPI-C" function chandle allocate_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  class Rig;
    chandle      pointer;
    virtual Port port;
    bit          started_null;

    function new(chandle c = null, virtual Port p = null);
      started_null = (pointer === null) && (port == null);
      pointer = c;
      port = p;
    endfunction
  endclass

  class Bare;
    chandle      pointer;
    virtual Port port;
  endclass

  Port wired ();

  chandle made;
  bit     given_started_null;
  bit     given_holds_pointer;
  int     given_reads;
  int     given_tag;
  bit     defaulted_started_null;
  bit     defaulted_pointer_is_null;
  bit     defaulted_port_is_null;
  bit     bare_pointer_is_null;
  bit     bare_port_is_null;

  initial begin
    Rig given;
    Rig defaulted;
    Bare bare;

    given_started_null = 1'b0;
    given_holds_pointer = 1'b0;
    given_reads = -1;
    given_tag = -1;
    defaulted_started_null = 1'b0;
    defaulted_pointer_is_null = 1'b0;
    defaulted_port_is_null = 1'b0;
    bare_pointer_is_null = 1'b0;
    bare_port_is_null = 1'b0;

    wired.tag = 9;
    made = allocate_cell(4);

    given = new(made, wired);
    given_started_null = given.started_null;
    given_holds_pointer = (given.pointer === made);
    given_reads = read_cell(given.pointer);
    given_tag = given.port.tag;

    defaulted = new;
    defaulted_started_null = defaulted.started_null;
    defaulted_pointer_is_null = (defaulted.pointer === null);
    defaulted_port_is_null = (defaulted.port == null);

    bare = new;
    bare_pointer_is_null = (bare.pointer === null);
    bare_port_is_null = (bare.port == null);

    release_cell(made);
  end

  final begin
    if (given_started_null !== 1'b1)
      $fatal(1, "the properties were not null when the constructor began");
    if (given_holds_pointer !== 1'b1) $fatal(1, "the constructor did not store its chandle");
    if (given_reads !== 4) $fatal(1, "the stored chandle reached %0d, expected 4", given_reads);
    if (given_tag !== 9) $fatal(1, "the stored virtual interface reached tag %0d, expected 9", given_tag);
    if (defaulted_started_null !== 1'b1)
      $fatal(1, "the properties were not null when a defaulted constructor began");
    if (defaulted_pointer_is_null !== 1'b1) $fatal(1, "a defaulted chandle was not null");
    if (defaulted_port_is_null !== 1'b1) $fatal(1, "a defaulted virtual interface was not null");
    if (bare_pointer_is_null !== 1'b1) $fatal(1, "a chandle no constructor set was not null");
    if (bare_port_is_null !== 1'b1) $fatal(1, "a virtual interface no constructor set was not null");
    $display("All checks passed");
  end
endmodule
