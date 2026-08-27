// Two string operands are equal when they hold the same characters (LRM 6.16),
// so a case statement over a string selects the item whose string literal holds
// those characters, and the default item when no literal does (LRM 12.5).
module Top;
  string word;
  int single;
  int from_list;
  int defaulted;

  initial begin
    word = "hello";
    single = 0;
    case (word)
      "hi":    single = 1;
      "hello": single = 2;
      "bye":   single = 3;
      default: single = 99;
    endcase

    word = "world";
    from_list = 0;
    case (word)
      "hi":             from_list = 1;
      "hello", "world": from_list = 23;
      default:          from_list = 99;
    endcase

    word = "unknown";
    defaulted = 0;
    case (word)
      "alpha": defaulted = 1;
      "beta":  defaulted = 2;
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
