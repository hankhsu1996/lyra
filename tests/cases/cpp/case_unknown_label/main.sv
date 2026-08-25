module Top;
  logic [3:0] sel;
  int x_self;
  int z_self;
  int x_vs_known;
  int known_vs_x;

  initial begin
    sel = 4'b10x1;
    x_self = 0;
    case (sel)
      4'b10x1: x_self = 1;
      default: x_self = 99;
    endcase

    sel = 4'b1z01;
    z_self = 0;
    case (sel)
      4'b1z01: z_self = 1;
      default: z_self = 99;
    endcase

    sel = 4'b10x1;
    x_vs_known = 0;
    case (sel)
      4'b1011: x_vs_known = 1;
      default: x_vs_known = 99;
    endcase

    sel = 4'b1011;
    known_vs_x = 0;
    case (sel)
      4'b10x1: known_vs_x = 1;
      default: known_vs_x = 99;
    endcase
  end
endmodule
