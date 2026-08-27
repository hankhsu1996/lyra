// A non-ANSI port list may give a port an external name that differs from the
// internal name it connects to (LRM 23.2.2.2). The two names then belong to
// different sides of the module: an instantiation connects by the external
// name, while the module's own body and a hierarchical name from outside both
// reach the internal declaration by its own name (LRM 23.6). A name that the
// module never put on a port is reached the same way, and so is one whose
// port is only its own storage seen from outside.
module Child (.ext_in(inner_in), .ext_out(inner_out));
  input var logic [7:0] inner_in;
  output var logic [7:0] inner_out;
  logic [7:0] unpublished;

  always_comb inner_out = inner_in + 8'd1;
  always_comb unpublished = inner_in * 8'd2;
endmodule

module Top;
  logic [7:0] src, dst;
  logic [7:0] seen_in, seen_out, seen_unpublished;

  Child u (.ext_in(src), .ext_out(dst));

  initial begin
    seen_in = 8'hff;
    seen_out = 8'hff;
    seen_unpublished = 8'hff;
    src = 8'd10;
    #1;
    seen_in = u.inner_in;
    seen_out = u.inner_out;
    seen_unpublished = u.unpublished;
  end

  final begin
    if (dst !== 8'd11) $fatal(1, "dst was %0d, expected 11", dst);
    if (seen_in !== 8'd10)
      $fatal(1, "u.inner_in was %0d, expected 10", seen_in);
    if (seen_out !== 8'd11)
      $fatal(1, "u.inner_out was %0d, expected 11", seen_out);
    if (seen_unpublished !== 8'd20)
      $fatal(1, "u.unpublished was %0d, expected 20", seen_unpublished);
    $display("All checks passed");
  end
endmodule
