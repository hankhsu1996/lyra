module Top;
  int gate;
  int count;
  initial begin
    gate = 1;
    count = 0;
    if (gate == 1) begin
      forever begin
        count = count + 1;
        if (count == 4) break;
      end
    end
  end
endmodule
