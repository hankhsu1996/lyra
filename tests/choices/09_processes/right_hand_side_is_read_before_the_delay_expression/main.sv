// An intra-assignment delay delays the assignment, and the right-hand side is
// evaluated "before the delay" (LRM 9.4.5). Whether that means before the
// delay expression is evaluated, or only before the time it names elapses, the
// standard does not settle, and Table 9-3 shows only constant delays. Lyra
// reads the right-hand side first, and for a nonblocking assignment the
// left-hand side's index as well, then evaluates the delay expression, so a
// delay expression that writes what the statement reads reaches neither. This
// case holds Lyra to that, for an integer, a string, a value too wide for a
// machine word, a static local, a property of an object the delay reaches
// through another handle, and a blocking assignment.
module Top;
  int evaluated [$];
  int read_first = 0;
  function automatic int right_side();
    evaluated.push_back(1);
    return 5;
  endfunction
  function automatic int delay_amount();
    evaluated.push_back(2);
    return 3;
  endfunction
  initial read_first <= #(delay_amount()) right_side();

  int changed_by_delay = 5;
  int index_before_delay = 1;
  int landed [4] = '{0, 0, 0, 0};
  function automatic int delay_that_writes();
    changed_by_delay = 99;
    index_before_delay = 3;
    return 2;
  endfunction
  initial landed[index_before_delay] <= #(delay_that_writes()) changed_by_delay;

  string text_source = "before";
  string text_landed = "";
  string text_blocking = "";
  logic [3999:0] wide_source = '1;
  logic [3999:0] wide_landed = '0;
  function automatic int delay_that_writes_text();
    text_source = "after";
    return 2;
  endfunction
  function automatic int delay_that_writes_wide();
    wide_source = '0;
    return 2;
  endfunction
  initial text_landed <= #(delay_that_writes_text()) text_source;
  initial wide_landed <= #(delay_that_writes_wide()) wide_source;

  string static_landed = "";
  function automatic int delay_that_writes_string(ref string s);
    s = "after";
    return 2;
  endfunction
  initial begin
    static string kept = "before";
    static_landed <= #(delay_that_writes_string(kept)) kept;
  end

  class Holder;
    string text;
  endclass
  Holder shared_holder;
  string property_landed = "";
  function automatic int delay_that_writes_property();
    shared_holder.text = "after";
    return 2;
  endfunction
  initial begin
    automatic Holder held = new;
    held.text = "before";
    shared_holder = held;
    property_landed <= #(delay_that_writes_property()) held.text;
  end
  initial begin
    automatic string from = "blocking before";
    text_blocking = #(clobber(from)) from;
  end
  function automatic int clobber(ref string s);
    s = "blocking after";
    return 1;
  endfunction

  final begin
    if (evaluated.size() !== 2 || evaluated[0] !== 1 || evaluated[1] !== 2)
      $fatal(1, "the right-hand side and the delay were evaluated in the order %p, expected '{1, 2}",
             evaluated);
    if (read_first !== 5)
      $fatal(1, "the delayed update stored %0d, expected 5", read_first);
    if (landed[1] !== 5 || landed[3] !== 0)
      $fatal(1, "a delay that wrote what the statement read reached the update: %p", landed);
    if (text_landed != "before")
      $fatal(1, "a delay that wrote a string reached the update: \"%s\"", text_landed);
    if (wide_landed !== {4000{1'b1}})
      $fatal(1, "a delay that wrote a wide value reached the update: %h", wide_landed[7:0]);
    if (static_landed != "before")
      $fatal(1, "a delay that wrote a static local reached the update: \"%s\"",
             static_landed);
    if (property_landed != "before")
      $fatal(1, "a delay that wrote a property reached the update: \"%s\"",
             property_landed);
    if (text_blocking != "blocking before")
      $fatal(1, "a delay that wrote a string reached the blocking assignment: \"%s\"",
             text_blocking);
    $display("All checks passed");
  end
endmodule
