// A module declaration may give a default value for a singular input port, and
// an instantiation that omits that port gets the default inserted (LRM
// 23.2.2.4). An explicit connection expression is used in place of the
// default, while an explicit empty named connection means the opposite of
// omitting the port: it leaves the port unconnected and the default is not
// used (LRM 23.3.2.2). A port left unconnected holds the default initial value
// of its data type (LRM 23.3.3.2), which is what an input with no declared
// default also holds when the instantiation omits it.
//
// A default is evaluated in the scope of the module that declares it, so it
// reads that instance's own parameters, and it takes the port's type as any
// value assigned to the port does. The module's own names stay its own, whatever
// they are spelled as.
module Child (
    input int din = 171, input int ein, input int cin, output int dout);
  assign dout = din + ein + cin;
endmodule

module Scaled #(parameter int K = 0) (
    input int scaled = K * 3,
    input logic [7:0] narrow = K * 100,
    output int got_scaled,
    output int got_narrow
);
  function automatic int port_default_scaled();
    return 1000;
  endfunction

  assign got_scaled = scaled + port_default_scaled();
  assign got_narrow = narrow;
endmodule

module Top;
  int c;
  int from_default;
  int from_expression;
  int from_empty;
  int scaled[1:3];
  int narrow[1:3];

  Child u_default (.cin(c), .dout(from_default));
  Child u_expression (.din(8), .ein(100), .cin(c), .dout(from_expression));
  Child u_empty (.din(), .cin(c), .dout(from_empty));

  for (genvar i = 1; i <= 3; i++) begin : g
    Scaled #(.K(i)) u (.got_scaled(scaled[i]), .got_narrow(narrow[i]));
  end

  initial c = 5;

  final begin
    if (from_default !== 176)
      $fatal(1, "from_default was %0d, expected 176", from_default);
    if (from_expression !== 113)
      $fatal(1, "from_expression was %0d, expected 113", from_expression);
    if (from_empty !== 5)
      $fatal(1, "from_empty was %0d, expected 5", from_empty);
    for (int i = 1; i <= 3; i++) begin
      if (scaled[i] !== 3 * i + 1000)
        $fatal(1, "g[%0d] defaulted scaled to %0d", i, scaled[i]);
      // 100, 200 and 300 kept to their low 8 bits.
      if (narrow[i] !== (100 * i) % 256)
        $fatal(1, "g[%0d] defaulted narrow to %0d", i, narrow[i]);
    end
    $display("All checks passed");
  end
endmodule
