module Top;
  int flag;
  int picked_true;
  int picked_false;
  initial begin
    flag = 1;
    picked_true = flag ? 42 : -1;
    picked_false = (flag == 0) ? 42 : -1;
  end
endmodule
