module Top;
  int x;
  int r;

  initial begin
    x = 5;
    r = 0;
    casez (x)
      32'b00000000000000000000000000000???: r = 1;
      32'd5: r = 2;
      default: r = 99;
    endcase
  end
endmodule
