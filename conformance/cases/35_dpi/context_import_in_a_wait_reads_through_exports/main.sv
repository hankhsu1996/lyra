// A wait on an expression that calls a context import reads what that import
// reads, and a context import reads SystemVerilog data by calling exports
// (LRM 35.5.3), so a write to what an export reads reevaluates the wait (LRM
// 9.4.2). That holds wherever the import is called -- in the waited expression,
// in a level wait, inside a function the expression calls -- and for an export
// of another instance that the foreign code reaches by setting its scope, and
// for what a function the export calls reads in turn. Finding out what the
// import reads runs it no more often than the evaluations do.
module Other;
  int held = 0;

  export "DPI-C" function read_held;
  function int read_held();
    return held;
  endfunction
endmodule

module Top;
  Other other ();

  int g = 0;
  int level = 0;
  int deep = 0;

  export "DPI-C" function read_g;
  function int read_g();
    return g;
  endfunction

  export "DPI-C" function read_level;
  function int read_level();
    return level;
  endfunction

  function int deep_value();
    return deep;
  endfunction

  export "DPI-C" function read_deep;
  function int read_deep();
    return deep_value();
  endfunction

  import "DPI-C" context function int g_via_c();
  import "DPI-C" context function int level_via_c();
  import "DPI-C" context function int deep_via_c();
  import "DPI-C" context function int held_via_c(input string path);
  import "DPI-C" function int g_via_c_calls();

  function int wrapped();
    return g_via_c() + 0;
  endfunction

  time direct_woke = 0;
  time level_woke = 0;
  time wrapped_woke = 0;
  time other_woke = 0;
  time deep_woke = 0;
  int calls_before;
  int calls_after;

  initial begin
    fork
      begin @(g_via_c()); direct_woke = $time; end
      begin wait (level_via_c() == 2); level_woke = $time; end
      begin @(wrapped()); wrapped_woke = $time; end
      begin @(held_via_c("Top.other")); other_woke = $time; end
      begin @(deep_via_c()); deep_woke = $time; end
    join_none
    #5 g = 1;
    #5 level = 1;
    #5 level = 2;
    #5 other.held = 3;
    #5 deep = 4;
    // One wait, one change: an evaluation as it starts and one as the change
    // reaches it, and the import runs once in each.
    #5;
    calls_before = g_via_c_calls();
    fork
      begin @(g_via_c()); end
    join_none
    #1 g = 2;
    #1 calls_after = g_via_c_calls();
  end

  final begin
    if (direct_woke !== 5)
      $fatal(1, "a context import in the waited expression ended the wait at %0t, expected 5",
             direct_woke);
    if (level_woke !== 15)
      $fatal(1, "a level wait on a context import ended at %0t, expected 15", level_woke);
    if (wrapped_woke !== 5)
      $fatal(1, "a context import a function calls ended the wait at %0t, expected 5",
             wrapped_woke);
    if (other_woke !== 20)
      $fatal(1, "an export of another instance ended the wait at %0t, expected 20", other_woke);
    if (deep_woke !== 25)
      $fatal(1, "what an export's own call reads ended the wait at %0t, expected 25", deep_woke);
    if (calls_after - calls_before !== 2)
      $fatal(1, "the import ran %0d times for one wait and one change, expected 2",
             calls_after - calls_before);
    $display("All checks passed");
  end
endmodule
