// @top: cfg
//
// The format specification %l, or %L, takes no argument and prints the
// library binding information of the module instance containing the task that
// formats it, as "library.cell": the library the instance's cell was taken
// from and the name of that cell (LRM 21.2.1.2, 33.7). A source file no
// library declaration matches is compiled into the library named work (LRM
// 33.3.1). The information is the cell's, so every instance of one cell
// prints the same text whatever its parameters are set to and wherever in
// the hierarchy it stands, two cells of one name in two libraries print
// different text, and a task, a named block or a generate block inside a
// module prints that module's. An argument is required for every % of a
// format string but %m, %l and %% (LRM 21.2.1.2), so %l leaves the arguments
// to the specifications around it, in a format string known only when it is
// formatted (LRM 21.3.3) as in a literal one.
module Top;
  string at_top = "unset";
  string upper_case = "unset";
  string in_named = "unset";
  string in_function = "unset";
  string among_arguments = "unset";
  string computed_format = "%l/%0d";
  string from_computed = "unset";

  Same from_lib1 ();
  Same from_lib2 ();
  Sized #(.Width(1)) narrow ();
  Sized #(.Width(9)) wide ();

  function automatic string stamp();
    return $sformatf("%l");
  endfunction

  initial begin
    at_top = $sformatf("%l");
    upper_case = $sformatf("%L");
    begin : named
      in_named = $sformatf("%l");
    end
    in_function = stamp();
    among_arguments = $sformatf("%0d %l %0d", 4, 5);
    from_computed = $sformatf(computed_format, 7);
  end

  for (genvar i = 0; i < 2; i++) begin : g_repeated
    string in_generate = "unset";
    initial in_generate = $sformatf("%l");
  end

  final begin
    if (at_top != "work.Top")
      $fatal(1, "%%l in the top module was %s, expected work.Top", at_top);
    if (upper_case != "work.Top")
      $fatal(1, "%%L in the top module was %s, expected work.Top", upper_case);
    if (in_named != "work.Top")
      $fatal(1, "%%l in a named block was %s, expected work.Top", in_named);
    if (in_function != "work.Top")
      $fatal(1, "%%l in a function was %s, expected work.Top", in_function);
    if (g_repeated[1].in_generate != "work.Top")
      $fatal(1, "%%l in a generate block was %s, expected work.Top",
             g_repeated[1].in_generate);
    if (among_arguments != "4 work.Top 5")
      $fatal(1, "%%l among arguments gave '%s', expected '4 work.Top 5'",
             among_arguments);
    if (from_computed != "work.Top/7")
      $fatal(1, "%%l in a computed format gave '%s', expected 'work.Top/7'",
             from_computed);
    if (from_lib1.binding != "lib1.Same")
      $fatal(1, "from_lib1 printed %s, expected lib1.Same", from_lib1.binding);
    if (from_lib2.binding != "lib2.Same")
      $fatal(1, "from_lib2 printed %s, expected lib2.Same", from_lib2.binding);
    if (narrow.binding != "lib1.Sized")
      $fatal(1, "narrow printed %s, expected lib1.Sized", narrow.binding);
    if (wide.binding != "lib1.Sized")
      $fatal(1, "wide printed %s, expected lib1.Sized", wide.binding);
    $display("All checks passed");
  end
endmodule

config cfg;
  design work.Top;
  default liblist work lib1;
  instance Top.from_lib2 use lib2.Same;
endconfig
