module Top;
  string s;
  int single;
  int multi;
  int defaulted;

  initial begin
    single = 0;
    multi = 0;
    defaulted = 0;

    s = "hello";
    case (s)
      "hi":    single = 1;
      "hello": single = 2;
      "bye":   single = 3;
      default: single = 99;
    endcase

    s = "world";
    case (s)
      "hi":             multi = 1;
      "hello", "world": multi = 23;
      default:          multi = 99;
    endcase

    s = "unknown";
    case (s)
      "alpha":  defaulted = 1;
      "beta":   defaulted = 2;
      default:  defaulted = 100;
    endcase
  end
endmodule
