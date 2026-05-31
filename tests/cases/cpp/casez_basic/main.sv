module Top;
  int x;
  int r1;
  int r2;

  initial begin
    x = 10;
    r1 = 0;
    casez (x)
      32'b1???????????????????????????????: r1 = 1;
      32'b0000000000000000000000000000????: r1 = 2;
      default: r1 = 99;
    endcase

    x = 7;
    r2 = 0;
    casez (x)
      32'b00000000000000000000000000000???: r2 = 1;
      32'b0000000000000000000000000000????: r2 = 2;
      default: r2 = 99;
    endcase
  end
endmodule
