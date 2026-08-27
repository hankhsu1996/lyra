// An enumerated type declares a set of integral named constants (LRM 6.19), so
// a case statement over a variable of that type compares it against those
// constants as integral values and selects the item naming the one it holds
// (LRM 12.5).
module Top;
  typedef enum {RED, GREEN, BLUE} Color;

  Color shade;
  int single;
  int from_list;
  int defaulted;

  initial begin
    shade = GREEN;
    single = 0;
    case (shade)
      RED:     single = 1;
      GREEN:   single = 2;
      BLUE:    single = 3;
      default: single = 99;
    endcase

    shade = BLUE;
    from_list = 0;
    case (shade)
      RED:         from_list = 1;
      GREEN, BLUE: from_list = 23;
      default:     from_list = 99;
    endcase

    shade = RED;
    defaulted = 0;
    case (shade)
      GREEN:   defaulted = 1;
      BLUE:    defaulted = 2;
      default: defaulted = 100;
    endcase
  end

  final begin
    if (single !== 2) $fatal(1, "single was %0d, expected 2", single);
    if (from_list !== 23)
      $fatal(1, "from_list was %0d, expected 23", from_list);
    if (defaulted !== 100)
      $fatal(1, "defaulted was %0d, expected 100", defaulted);
    $display("All checks passed");
  end
endmodule
