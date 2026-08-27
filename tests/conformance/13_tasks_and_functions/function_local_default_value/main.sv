// A variable declared inside a function with no initializer holds the default
// initial value of its type before anything is assigned to it: zero for a
// two-state integral type, x for a four-state one, the empty string for a
// string, and the element type's default for an element of an unpacked array
// (LRM 6.8).
module Top;
  int from_int;
  logic [7:0] from_logic;
  string from_string;
  int from_array;

  function automatic int probe_int;
    int x;
    return x;
  endfunction

  function automatic logic [7:0] probe_logic;
    logic [7:0] y;
    return y;
  endfunction

  function automatic string probe_string;
    string s;
    return s;
  endfunction

  function automatic int probe_array;
    int a[3];
    return a[1];
  endfunction

  initial begin
    from_int = 123;
    from_logic = 8'h55;
    from_string = "seeded";
    from_array = 456;

    from_int = probe_int();
    from_logic = probe_logic();
    from_string = probe_string();
    from_array = probe_array();
  end

  final begin
    if (from_int !== 0) $fatal(1, "from_int was %0d, expected 0", from_int);
    if (from_logic !== 8'hxx)
      $fatal(1, "from_logic was %h, expected xx", from_logic);
    if (from_string !== "")
      $fatal(1, "from_string was \"%s\", expected empty", from_string);
    if (from_array !== 0)
      $fatal(1, "from_array was %0d, expected 0", from_array);
    $display("All checks passed");
  end
endmodule
