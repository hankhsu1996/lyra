module Top;
  int sel;
  int hi;
  int lo;
  int result;
  initial begin
    sel = 1;
    hi = 100;
    lo = 1;
    result = 10 + (sel == 1 ? hi : lo) * 2;
  end
endmodule
