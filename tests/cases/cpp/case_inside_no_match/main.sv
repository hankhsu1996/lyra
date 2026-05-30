module Top;
  int val;
  int r;

  initial begin
    val = 100;
    r = 7;
    case (val) inside
      [1:3]:  r = 1;
      [4:6]:  r = 2;
    endcase
  end
endmodule
