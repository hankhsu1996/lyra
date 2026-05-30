module Top;
  typedef enum {RED, GREEN, BLUE} Color;

  int single;
  int multi;
  int defaulted;

  initial begin
    single = 0;
    multi = 0;
    defaulted = 0;

    case (GREEN)
      RED:   single = 1;
      GREEN: single = 2;
      BLUE:  single = 3;
      default: single = 99;
    endcase

    case (BLUE)
      RED:         multi = 1;
      GREEN, BLUE: multi = 23;
      default:     multi = 99;
    endcase

    case (RED)
      GREEN:   defaulted = 1;
      BLUE:    defaulted = 2;
      default: defaulted = 100;
    endcase
  end
endmodule
