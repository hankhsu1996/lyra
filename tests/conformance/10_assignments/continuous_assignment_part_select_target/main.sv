// A constant part-select of a packed variable is a legal left-hand side for a
// continuous assignment (LRM 10.3.2, Table 10-1). Each bit of a packed type is
// an independent element (LRM 6.5), so the assignment drives the selected bits
// and only those, tracking its operands there while the rest of the variable
// keeps the default value of its data type (LRM 6.8).
module Top;
  logic [3:0] source;
  logic [7:0] target;

  assign target[3:0] = source;

  logic [3:0] first;
  logic [3:0] second;

  initial begin
    source = 4'h5;
    #1;
    first = target[3:0];
    source = 4'hC;
    #1;
    second = target[3:0];
  end

  final begin
    if (first !== 4'h5) $fatal(1, "first was %h, expected 5", first);
    if (second !== 4'hC) $fatal(1, "second was %h, expected c", second);
    if (target[7:4] !== 4'bxxxx)
      $fatal(1, "target[7:4] was %b, expected xxxx", target[7:4]);
    $display("All checks passed");
  end
endmodule
