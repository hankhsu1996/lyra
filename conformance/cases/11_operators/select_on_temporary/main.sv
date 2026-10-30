// A concatenation is an operand, and so is a function call, so a bit-select or
// a part-select addresses the value one of them produces exactly as it
// addresses a named vector. An index or a base need not be constant: only the
// width of an indexed part-select must be (LRM 11.5, 11.5.1).
module Top;
  logic [7:0] high;
  logic [7:0] low;
  int words[4];

  logic [3:0] concatenated_range;
  logic concatenated_bit;
  logic [3:0] replicated_range;
  logic [3:0] concatenated_indexed;
  int computed_index;
  logic call_result_bit;
  logic [3:0] call_result_range;

  function automatic logic [7:0] make_packed();
    return 8'hAB;
  endfunction

  initial begin
    high = 8'h3C;
    low = 8'h9A;

    concatenated_range = {high, low}[11:8];
    concatenated_bit = {high, low}[11];
    replicated_range = {2{8'hA5}}[11:8];
    concatenated_indexed = {high, low}[4+:4];

    words[0] = 10;
    words[1] = 20;
    words[2] = 30;
    words[3] = 40;
    computed_index = words[1+1];

    call_result_bit = make_packed()[3];
    call_result_range = make_packed()[7:4];
  end

  final begin
    if (concatenated_range !== 4'hC)
      $fatal(1, "bits 11:8 of 3c9a were %h, expected c", concatenated_range);
    if (concatenated_bit !== 1'b1)
      $fatal(1, "bit 11 of 3c9a was %b, expected 1", concatenated_bit);
    if (replicated_range !== 4'h5)
      $fatal(1, "bits 11:8 of a5a5 were %h, expected 5", replicated_range);
    if (concatenated_indexed !== 4'h9)
      $fatal(1, "bits 7:4 of 3c9a were %h, expected 9", concatenated_indexed);
    if (computed_index !== 30)
      $fatal(1, "the element at a computed index was %0d, expected 30",
             computed_index);
    if (call_result_bit !== 1'b1)
      $fatal(1, "bit 3 of ab was %b, expected 1", call_result_bit);
    if (call_result_range !== 4'hA)
      $fatal(1, "bits 7:4 of ab were %h, expected a", call_result_range);
    $display("All checks passed");
  end
endmodule
