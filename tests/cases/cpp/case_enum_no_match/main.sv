module Top;
  typedef enum {ALPHA, BETA, GAMMA} Tag;

  Tag t;
  int r;

  initial begin
    r = 7;
    t = GAMMA;
    case (t)
      ALPHA: r = 1;
      BETA:  r = 2;
    endcase
  end
endmodule
