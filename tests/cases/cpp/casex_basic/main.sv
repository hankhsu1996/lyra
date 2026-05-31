module Top;
  int x;
  int r;
  int r2;

  initial begin
    x = 5;
    r = 0;
    casex (x)
      32'b00000000000000000000000000000xxx: r = 1;
      32'd10: r = 2;
      default: r = 99;
    endcase

    x = 15;
    r2 = 0;
    casex (x)
      32'bxxxx: r2 = 1;
      default: r2 = 99;
    endcase
  end
endmodule
