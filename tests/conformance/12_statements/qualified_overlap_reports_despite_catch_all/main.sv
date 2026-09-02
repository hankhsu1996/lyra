// @reports: violation
//
// A unique-if and a unique0-if are violated when more than one condition is
// found true, and a unique-case and a unique0-case when more than one case_item
// matches. What an explicit else or a default covers is the values no arm
// claimed, so it answers for whether some arm holds and never for whether two
// do: overlap is reported with a catch-all present exactly as without one. The
// statement that runs is still the first matching arm's and only that one's
// (LRM 12.4.2, 12.5.3).
module Top;
  int if_taken;
  int case_taken;

  initial begin
    bit [2:0] value;
    value = 3'd3;

    if_taken = 0;
    unique if (value[0] == 1) if_taken = 1;
    else if (value[1] == 1) if_taken = 2;
    else if_taken = 7;

    case_taken = 0;
    unique0 casez (value)
      3'b0?1: case_taken = 1;
      3'b01?: case_taken = 2;
      default: case_taken = 7;
    endcase
  end

  final begin
    if (if_taken !== 1) $fatal(1, "if_taken was %0d, expected 1", if_taken);
    if (case_taken !== 1)
      $fatal(1, "case_taken was %0d, expected 1", case_taken);
    $display("All checks passed");
  end
endmodule
