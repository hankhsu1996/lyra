module Top;
  event done;
  int sentinel;

  initial begin
    sentinel = 0;
    @done;
    sentinel = 1;
  end

  initial begin
    #5;
    -> done;
  end
endmodule
