// A loop generate's blocks each select the bit of a vector their own index
// names, and the last two blocks name bits past its end. A select outside its
// object reads the value a nonexistent entry has, `x` for a 4-state vector
// (LRM 11.5.1, 7.4.6), whatever the vector holds; the blocks inside the vector
// read their bit, and keep following it as it changes (LRM 10.3.2).
module Top;
  logic [3:0] a;
  logic y [6];

  for (genvar i = 0; i < 6; i += 1) begin : g
    assign y[i] = a[i];
  end

  initial begin
    a = 4'b1010;
    #1;
    for (int i = 0; i < 4; i++)
      if (y[i] !== a[i]) $fatal(1, "y[%0d] is %b with a %b", i, y[i], a);
    if (y[4] !== 1'bx || y[5] !== 1'bx)
      $fatal(1, "past the end y[4] is %b and y[5] is %b", y[4], y[5]);

    a = 4'b0101;
    #1;
    for (int i = 0; i < 4; i++)
      if (y[i] !== a[i]) $fatal(1, "after a change y[%0d] is %b", i, y[i]);
    if (y[4] !== 1'bx || y[5] !== 1'bx)
      $fatal(1, "after a change y[4] is %b and y[5] is %b", y[4], y[5]);
    $display("All checks passed");
  end
endmodule
