// A fixed-size unpacked array is a variable like any other, so one declared
// without an initializer holds, in every element, the default initial value
// of its element type: zero for a 2-state integral type, all x for a 4-state
// one, and the empty string for a string. Nothing about the array itself
// enters into it, so the same value reaches every element at every dimension
// of a multidimensional array, and an element that is a structure takes each
// of its members from that member's own type (LRM 6.8, Table 6-7, 7.2.2,
// 7.4.2).
module Top;
  typedef struct {
    int count;
    logic [3:0] mask;
  } entry_t;

  int two_state [3];
  logic [7:0] four_state [3];
  integer wide [2];
  string text [2];
  logic [3:0] grid [2][2];
  entry_t records [2];

  int two_state_first = 77;
  int two_state_last = 77;
  logic [7:0] four_state_first = 8'h5A;
  logic [7:0] four_state_last = 8'h5A;
  integer wide_second = 32'd77;
  string text_second = "unset";
  logic [3:0] grid_off_diagonal = 4'h5;
  logic [3:0] grid_last = 4'h5;
  int record_count = 77;
  logic [3:0] record_mask = 4'h5;

  initial begin
    two_state_first = two_state[0];
    two_state_last = two_state[2];

    four_state_first = four_state[0];
    four_state_last = four_state[2];

    wide_second = wide[1];
    text_second = text[1];

    grid_off_diagonal = grid[0][1];
    grid_last = grid[1][1];

    record_count = records[1].count;
    record_mask = records[1].mask;
  end

  final begin
    if (two_state_first !== 0)
      $fatal(1, "two_state_first was %0d, expected 0", two_state_first);
    if (two_state_last !== 0)
      $fatal(1, "two_state_last was %0d, expected 0", two_state_last);

    if (four_state_first !== 8'bxxxxxxxx)
      $fatal(1, "four_state_first was %0h, expected all x", four_state_first);
    if (four_state_last !== 8'bxxxxxxxx)
      $fatal(1, "four_state_last was %0h, expected all x", four_state_last);

    if (wide_second !== 32'bx)
      $fatal(1, "wide_second was %0h, expected all x", wide_second);

    if (text_second != "")
      $fatal(1, "text_second was '%s', expected the empty string",
             text_second);

    if (grid_off_diagonal !== 4'bxxxx)
      $fatal(1, "grid_off_diagonal was %0h, expected all x",
             grid_off_diagonal);
    if (grid_last !== 4'bxxxx)
      $fatal(1, "grid_last was %0h, expected all x", grid_last);

    if (record_count !== 0)
      $fatal(1, "record_count was %0d, expected 0", record_count);
    if (record_mask !== 4'bxxxx)
      $fatal(1, "record_mask was %0h, expected all x", record_mask);
    $display("All checks passed");
  end
endmodule
