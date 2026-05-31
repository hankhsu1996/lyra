module Top;
  int x;
  int r;
  int r2;

  initial begin
    x = 100;
    r = 0;
    casez (x)
      32'd1: r = 1;
      32'd2: r = 2;
      default: r = 99;
    endcase

    r2 = 0;
    casez (x)
      32'd1: r2 = 1;
      32'd2: r2 = 2;
    endcase
  end
endmodule
