// A string crosses a subroutine boundary under the same direction rules as an
// integral value: an input string is copied in at the call and the caller's
// string is untouched by what the body does to the formal, an output string is
// copied out at the return, an inout string is copied both ways, and a
// returned string carries whatever the body built (LRM 13.4, 13.5, 13.5.1).
module Top;
  string given;
  string greeting;
  string filled;
  string appended;

  function automatic string greet(input string name);
    name = {"Hello, ", name};
    return name;
  endfunction

  function automatic void fill(output string s);
    s = "filled";
  endfunction

  function automatic void append_bang(inout string s);
    s = {s, "!"};
  endfunction

  initial begin
    given = "World";
    greeting = greet(given);

    fill(filled);

    appended = "hi";
    append_bang(appended);
  end

  final begin
    if (greeting !== "Hello, World")
      $fatal(1, "greeting was \"%s\", expected \"Hello, World\"", greeting);
    if (given !== "World")
      $fatal(1, "given was \"%s\", expected \"World\"", given);
    if (filled !== "filled")
      $fatal(1, "filled was \"%s\", expected \"filled\"", filled);
    if (appended !== "hi!")
      $fatal(1, "appended was \"%s\", expected \"hi!\"", appended);
    $display("All checks passed");
  end
endmodule
