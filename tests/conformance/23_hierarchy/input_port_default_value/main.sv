// A module declaration may give a default value for a singular input port, and
// an instantiation that omits that port gets the default inserted (LRM
// 23.2.2.4). An explicit connection expression is used in place of the
// default, while an explicit empty named connection means the opposite of
// omitting the port: it leaves the port unconnected and the default is not
// used (LRM 23.3.2.2). A port left unconnected holds the default initial value
// of its data type (LRM 23.3.3.2), which is what an input with no declared
// default also holds when the instantiation omits it.
module Child (
    input int din = 171, input int ein, input int cin, output int dout);
  assign dout = din + ein + cin;
endmodule

module Top;
  int c;
  int from_default;
  int from_expression;
  int from_empty;

  Child u_default (.cin(c), .dout(from_default));
  Child u_expression (.din(8), .ein(100), .cin(c), .dout(from_expression));
  Child u_empty (.din(), .cin(c), .dout(from_empty));

  initial c = 5;

  final begin
    if (from_default !== 176)
      $fatal(1, "from_default was %0d, expected 176", from_default);
    if (from_expression !== 113)
      $fatal(1, "from_expression was %0d, expected 113", from_expression);
    if (from_empty !== 5)
      $fatal(1, "from_empty was %0d, expected 5", from_empty);
    $display("All checks passed");
  end
endmodule
