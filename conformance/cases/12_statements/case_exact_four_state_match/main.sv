// A plain case statement's comparison succeeds only when each bit matches
// exactly with respect to the values 0, 1, x, and z (LRM 12.5). A bit of the
// case expression holding x or z therefore matches only an item bit holding
// that same value, and never one holding 0, 1, or the other unknown value.
module Top;
  logic [3:0] sel;
  int x_against_x;
  int z_against_z;
  int x_against_one;
  int one_against_x;
  int x_against_z;

  initial begin
    sel = 4'b10x1;
    x_against_x = 0;
    case (sel)
      4'b10x1: x_against_x = 1;
      default: x_against_x = 99;
    endcase

    sel = 4'b1z01;
    z_against_z = 0;
    case (sel)
      4'b1z01: z_against_z = 1;
      default: z_against_z = 99;
    endcase

    sel = 4'b10x1;
    x_against_one = 0;
    case (sel)
      4'b1011: x_against_one = 1;
      default: x_against_one = 99;
    endcase

    sel = 4'b1011;
    one_against_x = 0;
    case (sel)
      4'b10x1: one_against_x = 1;
      default: one_against_x = 99;
    endcase

    sel = 4'b10x1;
    x_against_z = 0;
    case (sel)
      4'b10z1: x_against_z = 1;
      default: x_against_z = 99;
    endcase
  end

  final begin
    if (x_against_x !== 1)
      $fatal(1, "x_against_x was %0d, expected 1", x_against_x);
    if (z_against_z !== 1)
      $fatal(1, "z_against_z was %0d, expected 1", z_against_z);
    if (x_against_one !== 99)
      $fatal(1, "x_against_one was %0d, expected 99", x_against_one);
    if (one_against_x !== 99)
      $fatal(1, "one_against_x was %0d, expected 99", one_against_x);
    if (x_against_z !== 99)
      $fatal(1, "x_against_z was %0d, expected 99", x_against_z);
    $display("All checks passed");
  end
endmodule
