module Top;
  int x;
  int y;

  task clamp(inout int v);
    if (v > 10) begin
      v = 10;
      return;
    end
    v = v + 100;
  endtask

  initial begin
    x = 50;
    clamp(x);
    y = 3;
    clamp(y);
  end
endmodule
