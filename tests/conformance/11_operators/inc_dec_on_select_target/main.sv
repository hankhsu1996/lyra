// The increment and decrement operators behave as blocking assignments, so
// their operand may be a packed array element, a bit-select or a
// part-select. The update is computed in the width of that operand and
// stored back into it, wrapping within it and leaving the rest of the value
// untouched (LRM 11.4.2, 11.5.1).
module Top;
  bit [3:0][7:0] elements;
  bit [15:0] halves;
  bit [7:0] wrapping;
  bit [7:0] single_bit;

  initial begin
    elements = 32'h11_22_33_44;
    elements[2]--;
    ++elements[1];

    halves = 16'h1234;
    halves[7:0]++;
    ++halves[15:8];

    wrapping = 8'h00;
    wrapping[3:0]--;

    single_bit = 8'b1010_1010;
    single_bit[0]++;
  end

  final begin
    if (elements[3] !== 8'h11)
      $fatal(1, "elements[3] was %h, expected 11", elements[3]);
    if (elements[2] !== 8'h21)
      $fatal(1, "elements[2]-- gave %h, expected 21", elements[2]);
    if (elements[1] !== 8'h34)
      $fatal(1, "++elements[1] gave %h, expected 34", elements[1]);
    if (elements[0] !== 8'h44)
      $fatal(1, "elements[0] was %h, expected 44", elements[0]);
    if (halves !== 16'h1335)
      $fatal(1, "incrementing both halves gave %h, expected 1335", halves);
    if (wrapping !== 8'h0F)
      $fatal(1, "[3:0]-- from zero gave %h, expected 0f", wrapping);
    if (single_bit !== 8'b1010_1011)
      $fatal(1, "[0]++ gave %b, expected 10101011", single_bit);
    $display("All checks passed");
  end
endmodule
