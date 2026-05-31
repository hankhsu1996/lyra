module Top;
  logic [3:0] sel;
  int r;
  int rmatch;

  initial begin
    sel = 4'b00xx;
    r = 0;
    casex (sel)
      4'b0000: r = 1;
      default: r = 99;
    endcase

    sel = 4'b01xz;
    rmatch = 0;
    casex (sel)
      4'b0100: rmatch = 1;
      default: rmatch = 99;
    endcase
  end
endmodule
