module Top;
  int x;
  int y;
  int r;

  initial begin
    x = 5;
    y = 4;
    r = 0;
    casez (x)
      32'b00000000000000000000000000000???: begin
        casez (y)
          32'b00000000000000000000000000000100: r = 10;
          default: r = 19;
        endcase
      end
      default: r = 99;
    endcase
  end
endmodule
