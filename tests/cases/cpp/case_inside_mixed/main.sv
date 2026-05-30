module Top;
  int val;
  int r;

  initial begin
    val = 7;
    r = 0;
    case (val) inside
      1, 2, 3:  r = 10;
      [4:6]:    r = 20;
      7:        r = 30;
      [10:20]:  r = 40;
      default:  r = 99;
    endcase
  end
endmodule
