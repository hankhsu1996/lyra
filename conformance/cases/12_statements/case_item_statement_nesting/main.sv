// A case item's statement may itself be a case statement, and the inner case
// runs only when the outer statement selected the item holding it (LRM 12.5).
// A do-not-care case nests the same way (LRM 12.5.1).
module Top;
  int outer;
  int inner;
  logic [3:0] outer_code;
  logic [3:0] inner_code;
  int plain;
  int wildcarded;

  initial begin
    outer = 1;
    inner = 2;
    plain = 0;
    case (outer)
      1: begin
        case (inner)
          1: plain = 11;
          2: plain = 12;
          default: plain = 19;
        endcase
      end
      2: begin
        case (inner)
          1: plain = 21;
          2: plain = 22;
          default: plain = 29;
        endcase
      end
      default: plain = 99;
    endcase

    outer_code = 4'b0101;
    inner_code = 4'b0100;
    wildcarded = 0;
    casez (outer_code)
      4'b01??: begin
        casez (inner_code)
          4'b0100: wildcarded = 10;
          default: wildcarded = 19;
        endcase
      end
      default: wildcarded = 99;
    endcase
  end

  final begin
    if (plain !== 12) $fatal(1, "plain was %0d, expected 12", plain);
    if (wildcarded !== 10)
      $fatal(1, "wildcarded was %0d, expected 10", wildcarded);
    $display("All checks passed");
  end
endmodule
