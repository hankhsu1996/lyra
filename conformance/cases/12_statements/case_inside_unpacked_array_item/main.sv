// A case-inside item is compared with the set membership operator, so an item
// that is an unpacked array matches when the case expression matches one of
// its elements, and an item whose comparison is unknown is not matched (LRM
// 12.5.4, 11.4.13).
module Top;
  int val;
  int low [3];
  int high [$];
  logic [3:0] nibble;
  logic [3:0] masks [2];
  int in_first_array;
  int in_second_array;
  int in_no_array;
  int beside_a_range;
  int unknown_falls_through;

  initial begin
    low = '{1, 2, 3};
    high = '{7, 8, 9};
    masks = '{4'b1x01, 4'b0000};

    val = 2;
    in_first_array = 0;
    case (val) inside
      low:     in_first_array = 1;
      high:    in_first_array = 2;
      default: in_first_array = 9;
    endcase

    val = 9;
    in_second_array = 0;
    case (val) inside
      low:     in_second_array = 1;
      high:    in_second_array = 2;
      default: in_second_array = 9;
    endcase

    val = 5;
    in_no_array = 0;
    case (val) inside
      low:     in_no_array = 1;
      high:    in_no_array = 2;
      default: in_no_array = 9;
    endcase

    val = 8;
    beside_a_range = 0;
    case (val) inside
      low, [4:6]: beside_a_range = 1;
      20, high:   beside_a_range = 2;
      default:    beside_a_range = 9;
    endcase

    // x000 against 1x01 is a mismatch and against 0000 is unknown, so the
    // item's answer is unknown and the next item is tried.
    nibble = 4'bx000;
    unknown_falls_through = 0;
    case (nibble) inside
      masks:   unknown_falls_through = 1;
      default: unknown_falls_through = 9;
    endcase
  end

  final begin
    if (in_first_array !== 1)
      $fatal(1, "in_first_array was %0d, expected 1", in_first_array);
    if (in_second_array !== 2)
      $fatal(1, "in_second_array was %0d, expected 2", in_second_array);
    if (in_no_array !== 9)
      $fatal(1, "in_no_array was %0d, expected 9", in_no_array);
    if (beside_a_range !== 2)
      $fatal(1, "beside_a_range was %0d, expected 2", beside_a_range);
    if (unknown_falls_through !== 9)
      $fatal(1, "unknown_falls_through was %0d, expected 9",
             unknown_falls_through);
    $display("All checks passed");
  end
endmodule
