module Top;
  int sel;
  int plain;
  int wild_default;
  int untouched;

  initial begin
    sel = 5;
    plain = 0;
    wild_default = 0;
    untouched = 99;

    case (sel)
      default: plain = 42;
    endcase

    casez (sel)
      default: wild_default = 7;
    endcase
  end
endmodule
