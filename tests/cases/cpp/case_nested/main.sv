module Top;
  int result;
  initial begin
    int x;
    int y;
    x = 1;
    y = 2;
    result = 0;
    case (x)
      1: begin
        case (y)
          1: result = 11;
          2: result = 12;
          default: result = 19;
        endcase
      end
      2: begin
        case (y)
          1: result = 21;
          2: result = 22;
        endcase
      end
      default: result = 99;
    endcase
  end
endmodule
