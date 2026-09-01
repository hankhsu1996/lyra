// An assignment pattern gives a packed array a value element for element,
// matched by the same rules that match an unpacked one: the expressions take
// the elements in the order the dimension declares them, an index key names
// the element it supplies, a replication stands for an entire dimension, and
// the default key reaches every element no index named. Because the array is
// packed, what results is the vector its elements form, the first expression
// landing in the most significant element -- so an index and a position run
// opposite ways whenever the dimension descends. A vector of bits is such an
// array, so a pattern fills it a bit at a time (LRM 10.9, 10.9.1, 7.4.1).
module Top;
  logic [3:0][7:0] positional = 32'h11223344;
  logic [3:0][7:0] filled = 32'h11223344;
  logic [3:0][7:0] replicated = 32'h11223344;
  logic [3:0][7:0] indexed_and_filled = 32'h11223344;
  logic [3:0][7:0] fully_indexed = 32'h11223344;
  logic [7:0] cleared = 8'hA5;
  logic [3:0] all_ones = 4'h6;

  initial begin
    positional = '{8'hAA, 8'hBB, 8'hCC, 8'hDD};
    filled = '{default: 8'h5A};
    replicated = '{4{8'hAB}};
    indexed_and_filled = '{2: 8'hEE, 0: 8'h99, default: 8'h5A};
    fully_indexed = '{3: 8'hAA, 2: 8'hBB, 1: 8'hCC, 0: 8'hDD};
    cleared = '{default: '0};
    all_ones = '{default: '1};
  end

  final begin
    if (positional[3] !== 8'hAA)
      $fatal(1, "positional[3] was %0h, expected aa", positional[3]);
    if (positional[0] !== 8'hDD)
      $fatal(1, "positional[0] was %0h, expected dd", positional[0]);
    if (positional !== 32'hAABBCCDD)
      $fatal(1, "positional was %0h, expected aabbccdd", positional);

    if (filled !== 32'h5A5A5A5A)
      $fatal(1, "filled was %0h, expected 5a5a5a5a", filled);

    if (replicated !== 32'hABABABAB)
      $fatal(1, "replicated was %0h, expected abababab", replicated);

    if (indexed_and_filled !== 32'h5AEE5A99)
      $fatal(1, "indexed_and_filled was %0h, expected 5aee5a99",
             indexed_and_filled);

    if (fully_indexed !== 32'hAABBCCDD)
      $fatal(1, "fully_indexed was %0h, expected aabbccdd", fully_indexed);

    if (cleared !== 8'h00)
      $fatal(1, "cleared was %0h, expected 00", cleared);
    if (all_ones !== 4'hF)
      $fatal(1, "all_ones was %0h, expected f", all_ones);
    $display("All checks passed");
  end
endmodule
