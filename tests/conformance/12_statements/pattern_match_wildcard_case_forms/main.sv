// Wherever two bits are compared during pattern matching, whether they are tag
// bits or member bits, the casez form ignores a z bit and the casex form
// ignores both z and x, on either side of the comparison, so an item whose
// pattern differs from the value only at a do-not-care position is selected
// (LRM 12.6.1).
module Top;
  typedef union tagged {
    logic [3:0] Code;
    void Empty;
  } coded_t;

  typedef struct {
    int         id;
    logic [3:0] flags;
  } entry_t;

  typedef union tagged {
    entry_t Entry;
    void    None;
  } slot_t;

  // A packed tagged union keeps its tag in its own most significant bits
  // (LRM 7.3.2), so a value converted from a vector can carry x or z there.
  typedef union tagged packed {
    logic [3:0] A;
    logic [3:0] B;
  } packed_t;

  coded_t c;
  slot_t s;
  packed_t p;
  int casez_ignores_a_z_tag_bit = -1;
  int casex_ignores_an_x_tag_bit = -1;
  int case_keeps_an_x_tag_bit = -1;
  int casez_ignores_z = -1;
  int casez_ignores_a_z_in_the_value = -1;
  int casez_keeps_x = -1;
  int casex_ignores_x = -1;
  int casex_ignores_an_x_in_the_value = -1;
  int casez_inside_a_structure = -1;
  int casez_two_state_member = -1;

  initial begin
    c = tagged Code 4'b1010;

    casez_ignores_z = 0;
    casez (c) matches
      tagged Code 4'b10zz : casez_ignores_z = 1;
      default             : casez_ignores_z = -1;
    endcase

    casex_ignores_x = 0;
    casex (c) matches
      tagged Code 4'b10xx : casex_ignores_x = 1;
      default             : casex_ignores_x = -1;
    endcase

    // casez skips a z and nothing else, so an x in the pattern is a bit the
    // value has to carry.
    casez_keeps_x = 0;
    casez (c) matches
      tagged Code 4'b10xx : casez_keeps_x = -1;
      default             : casez_keeps_x = 1;
    endcase

    c = tagged Code 4'b1z1z;
    casez_ignores_a_z_in_the_value = 0;
    casez (c) matches
      tagged Code 4'b1010 : casez_ignores_a_z_in_the_value = 1;
      default             : casez_ignores_a_z_in_the_value = -1;
    endcase

    c = tagged Code 4'b1x1x;
    casex_ignores_an_x_in_the_value = 0;
    casex (c) matches
      tagged Code 4'b1010 : casex_ignores_an_x_in_the_value = 1;
      default             : casex_ignores_an_x_in_the_value = -1;
    endcase

    s = tagged Entry '{7, 4'b0110};
    casez_inside_a_structure = 0;
    casez (s) matches
      tagged Entry '{.*, 4'b01zz} : casez_inside_a_structure = 1;
      default                     : casez_inside_a_structure = -1;
    endcase

    // A two-state member holds no z, so the do-not-care bits are the
    // pattern's alone.
    casez_two_state_member = 0;
    casez (s) matches
      tagged Entry '{32'b0000_0000_0000_0000_0000_0000_0000_01zz, .*} :
        casez_two_state_member = 1;
      default : casez_two_state_member = -1;
    endcase

    // The tag bits are compared like the member's, so a z there matches
    // whichever tag the first item names.
    p = packed_t'(5'bz0101);
    casez_ignores_a_z_tag_bit = 0;
    casez (p) matches
      tagged A 4'b0101 : casez_ignores_a_z_tag_bit = 1;
      tagged B 4'b0101 : casez_ignores_a_z_tag_bit = 2;
      default          : casez_ignores_a_z_tag_bit = -1;
    endcase

    p = packed_t'(5'bx0101);
    casex_ignores_an_x_tag_bit = 0;
    casex (p) matches
      tagged B 4'b0101 : casex_ignores_an_x_tag_bit = 1;
      default          : casex_ignores_an_x_tag_bit = -1;
    endcase

    case_keeps_an_x_tag_bit = 0;
    case (p) matches
      tagged A 4'b0101 : case_keeps_an_x_tag_bit = -1;
      tagged B 4'b0101 : case_keeps_an_x_tag_bit = -2;
      default          : case_keeps_an_x_tag_bit = 1;
    endcase
  end

  final begin
    if (casez_ignores_z !== 1)
      $fatal(1, "casez_ignores_z was %0d, expected 1", casez_ignores_z);
    if (casex_ignores_x !== 1)
      $fatal(1, "casex_ignores_x was %0d, expected 1", casex_ignores_x);
    if (casez_keeps_x !== 1)
      $fatal(1, "casez_keeps_x was %0d, expected 1", casez_keeps_x);
    if (casez_ignores_a_z_in_the_value !== 1)
      $fatal(1, "casez_ignores_a_z_in_the_value was %0d, expected 1",
             casez_ignores_a_z_in_the_value);
    if (casex_ignores_an_x_in_the_value !== 1)
      $fatal(1, "casex_ignores_an_x_in_the_value was %0d, expected 1",
             casex_ignores_an_x_in_the_value);
    if (casez_inside_a_structure !== 1)
      $fatal(1, "casez_inside_a_structure was %0d, expected 1",
             casez_inside_a_structure);
    if (casez_two_state_member !== 1)
      $fatal(1, "casez_two_state_member was %0d, expected 1",
             casez_two_state_member);
    if (casez_ignores_a_z_tag_bit !== 1)
      $fatal(1, "casez_ignores_a_z_tag_bit was %0d, expected 1",
             casez_ignores_a_z_tag_bit);
    if (casex_ignores_an_x_tag_bit !== 1)
      $fatal(1, "casex_ignores_an_x_tag_bit was %0d, expected 1",
             casex_ignores_an_x_tag_bit);
    if (case_keeps_an_x_tag_bit !== 1)
      $fatal(1, "case_keeps_an_x_tag_bit was %0d, expected 1",
             case_keeps_an_x_tag_bit);
    $display("All checks passed");
  end
endmodule
