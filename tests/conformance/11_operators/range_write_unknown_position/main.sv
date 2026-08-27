// A part-select whose address is x or z yields x when it is read and has no
// effect on the stored data when it is written, and an address is x as soon
// as any single bit of it is x or z. A bit-select of the same vector at the
// same unknown address behaves the same way (LRM 11.5.1).
module Top;
  logic [7:0] source;
  logic [3:0] unknown_idx;
  logic [3:0] partly_unknown_idx;
  logic [3:0] known_idx;
  logic [3:0] part_read;
  logic [7:0] part_target;
  logic [7:0] bit_target;
  logic [7:0] partly_unknown_target;
  logic [7:0] known_target;

  initial begin
    part_read = 4'b0000;

    source = 8'hA5;
    unknown_idx = 4'bxxxx;
    partly_unknown_idx = 4'b001z;
    known_idx = 4'd2;

    part_read = source[unknown_idx +: 4];

    part_target = 8'hA5;
    part_target[unknown_idx +: 4] = 4'b0000;

    bit_target = 8'hA5;
    bit_target[unknown_idx] = 1'b0;

    partly_unknown_target = 8'hA5;
    partly_unknown_target[partly_unknown_idx +: 4] = 4'b0000;

    known_target = 8'hA5;
    known_target[known_idx +: 4] = 4'b0000;
  end

  final begin
    if (part_read !== 4'bxxxx)
      $fatal(1, "read at an x address was %b, expected xxxx", part_read);
    if (part_target !== 8'hA5)
      $fatal(1, "part-select write at an x address gave %h, expected a5",
             part_target);
    if (bit_target !== 8'hA5)
      $fatal(1, "bit-select write at an x address gave %h, expected a5",
             bit_target);
    if (partly_unknown_target !== 8'hA5)
      $fatal(1, "write at an address with one z bit gave %h, expected a5",
             partly_unknown_target);
    if (known_target !== 8'b1000_0001)
      $fatal(1, "write at a known address gave %b, expected 10000001",
             known_target);
    $display("All checks passed");
  end
endmodule
