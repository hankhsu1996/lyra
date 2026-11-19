// A slice of an unpacked array selects contiguous elements (LRM 7.4.5), and an
// index outside the array reads as the element type's default and is not
// written (LRM 7.4.5, Table 7-1). What a slice that reaches past the end does
// the standard does not say. Lyra takes each position beyond the end as an
// out-of-bounds index of its own, so it reads as the default and a write
// leaves it alone, while the positions the array has take part as usual, and
// a slice starting before the first element is taken the same way. This case
// holds Lyra to that, for a 2-state and a 4-state element, and for an array
// that is a member of a union, whose elements lie in the union's storage
// (LRM 7.3).
module Top;
  int arr [] = '{10, 20, 30, 40, 50};
  logic [7:0] four_state [] = '{8'h11, 8'h22, 8'h33};
  int past_end [3] = '{-1, -1, -1};
  logic [7:0] past_end_4s [3] = '{8'h5A, 8'h5A, 8'h5A};
  int written_past_end [] = '{10, 20, 30, 40, 50};
  typedef union {
    logic [3:0] nibbles [4];
    logic [15:0] word;
  } nibbles_t;
  nibbles_t past_member_end;
  nibbles_t before_member_start;
  int at;

  initial begin
    past_end = arr[3 +: 3];
    past_end_4s = four_state[2 +: 3];
    written_past_end[3 +: 3] = '{100, 200, 300};
    past_member_end.nibbles = '{default: 4'h0};
    before_member_start.nibbles = '{default: 4'h0};
    at = 3;
    past_member_end.nibbles[at +: 2] = '{4'ha, 4'hb};
    at = -1;
    before_member_start.nibbles[at +: 2] = '{4'ha, 4'hb};
  end

  final begin
    if (past_end[0] !== 40 || past_end[1] !== 50 || past_end[2] !== 0)
      $fatal(1, "a read past the end gave %0d %0d %0d, expected 40 50 0",
             past_end[0], past_end[1], past_end[2]);
    if (past_end_4s[0] !== 8'h33 || past_end_4s[1] !== 8'bxxxxxxxx ||
        past_end_4s[2] !== 8'bxxxxxxxx)
      $fatal(1, "a 4-state read past the end gave %h %h %h, expected 33 xx xx",
             past_end_4s[0], past_end_4s[1], past_end_4s[2]);
    if (written_past_end.size() !== 5 || written_past_end[2] !== 30 ||
        written_past_end[3] !== 100 || written_past_end[4] !== 200)
      $fatal(1, "a write past the end left %p", written_past_end);
    if (past_member_end.nibbles[0] !== 4'h0 ||
        past_member_end.nibbles[1] !== 4'h0 ||
        past_member_end.nibbles[2] !== 4'h0 ||
        past_member_end.nibbles[3] !== 4'ha)
      $fatal(1, "a write past a member's end left %p", past_member_end.nibbles);
    if (before_member_start.nibbles[0] !== 4'hb ||
        before_member_start.nibbles[1] !== 4'h0 ||
        before_member_start.nibbles[2] !== 4'h0 ||
        before_member_start.nibbles[3] !== 4'h0)
      $fatal(1, "a write before a member's start left %p",
             before_member_start.nibbles);
    $display("All checks passed");
  end
endmodule
