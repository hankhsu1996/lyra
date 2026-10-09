// An exported subroutine is one global symbol, but the scope it runs in is the
// one the DPI call chain currently holds, so a module instantiated twice or a
// generate block replicated twice is reached through the same symbol and still
// answers with its own copy of the scope's variables (LRM 35.4, 35.5.3, Annex
// H.9.2). A call chain takes its scope from the declaration of the context
// import that started it, and foreign code redirects it to any other named
// instance scope before calling an export declared elsewhere (LRM 35.5.3).
module Sub #(parameter int ID = 0);
  int id;

  export "DPI-C" function read_id;
  function int read_id();
    return id;
  endfunction

  initial id = ID;
endmodule

module Held (input int seed);
  export "DPI-C" function read_id;
  function int read_id();
    return seed;
  endfunction

  import "DPI-C" context function int read_at(input string path);
  int own = -1;
  initial #2 own = read_at($sformatf("%m"));

  final
    if (own !== seed)
      $fatal(1, "%m read %0d through its own import, expected %0d", own, seed);
endmodule

// Two instances of this module are alike in everything but where they stand
// and what they are connected to, so the export and the import inside each are
// told apart only by its scope.
module Pair (input int seed);
  for (genvar i = 0; i < 1; i++) begin : slot
    Held held (.seed);
  end
endmodule

module Top;
  Sub #(.ID(10)) m0 ();
  Sub #(.ID(20)) m1 ();
  Pair p0 (.seed(30));
  Pair p1 (.seed(31));

  import "DPI-C" context function int read_at(input string path);

  int at_m0;
  int at_m1;
  int at_p0;
  int at_p1;
  int seen[2];

  initial begin
    #1;
    at_m0 = read_at("Top.m0");
    at_m1 = read_at("Top.m1");
    at_p0 = read_at("Top.p0.slot[0].held");
    at_p1 = read_at("Top.p1.slot[0].held");
  end

  for (genvar i = 0; i < 2; i++) begin : replica
    int tag = 100 + (i * 7);

    export "DPI-C" function read_tag;
    function int read_tag();
      return tag;
    endfunction

    import "DPI-C" context function int call_read();

    initial begin
      #1;
      seen[i] = call_read();
    end
  end

  final begin
    // Two answers that differ are what separates a scope chosen per call from
    // one instance standing in for every call.
    if (at_m0 !== 10) $fatal(1, "Top.m0 answered %0d, expected 10", at_m0);
    if (at_m1 !== 20) $fatal(1, "Top.m1 answered %0d, expected 20", at_m1);
    if (at_p0 !== 30) $fatal(1, "Top.p0's answered %0d, expected 30", at_p0);
    if (at_p1 !== 31) $fatal(1, "Top.p1's answered %0d, expected 31", at_p1);
    if (seen[0] !== 100)
      $fatal(1, "the first replica answered %0d, expected 100", seen[0]);
    if (seen[1] !== 107)
      $fatal(1, "the second replica answered %0d, expected 107", seen[1]);
    $display("All checks passed");
  end
endmodule
