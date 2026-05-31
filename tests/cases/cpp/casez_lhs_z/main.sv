module Top;
  logic [3:0] sel;
  int r;
  int rmatch;

  initial begin
    sel = 4'b00zz;
    r = 0;
    casez (sel)
      4'b0000: r = 1;
      default: r = 99;
    endcase

    sel = 4'b01zz;
    rmatch = 0;
    casez (sel)
      4'b0100: rmatch = 1;
      default: rmatch = 99;
    endcase
  end
endmodule
