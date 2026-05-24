module Top;
  int x;
  initial begin
    x = 0;
    #10;
    x = 1;
    $finish;
    x = 2;
  end
endmodule
