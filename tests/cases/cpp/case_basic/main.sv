module Top;
  int single;
  int multi;
  int defaulted;
  int unchanged;
  initial begin
    single = 0;
    multi = 0;
    defaulted = 0;
    unchanged = 99;

    case (3)
      3: single = 100;
      default: single = 999;
    endcase

    case (2)
      0: multi = 1;
      1, 2: multi = 22;
      3: multi = 3;
      default: multi = 999;
    endcase

    case (5)
      0: defaulted = 1;
      1: defaulted = 2;
      default: defaulted = 100;
    endcase

    case (5)
      0: unchanged = 1;
      1: unchanged = 2;
    endcase
  end
endmodule
