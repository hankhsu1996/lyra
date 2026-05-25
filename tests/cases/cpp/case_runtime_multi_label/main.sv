module Top;
  int sel;
  int a, b, c;
  initial begin
    a = 0;
    b = 0;
    c = 99;

    sel = 1;
    case (1)
      sel == 0, sel == 2: a = 11;
      sel == 1, sel == 3: a = 22;
      default: a = 99;
    endcase

    sel = 3;
    case (1)
      sel == 0, sel == 2: b = 11;
      sel == 1, sel == 3: b = 22;
      default: b = 99;
    endcase

    sel = 7;
    case (1)
      sel == 0, sel == 2: c = 11;
      sel == 1, sel == 3: c = 22;
    endcase
  end
endmodule
