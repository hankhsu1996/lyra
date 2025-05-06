module Test;
  int a, b, c, d;
  bit clk, reset;
  longint counter;

  initial begin
    a = 10;
    b = 20;
    c = 0;
    clk = 0;
    reset = 1;
    counter = 0;

    c = a + b;
    c = -a;
    a++;

    #5 reset = 0;

    #5 clk = 1;
    #5 clk = 0;
    #5 clk = 1;
    #5 clk = 0;

    $finish();
  end

  always_comb begin
    if (reset) begin
      d = 0;
    end
    else begin
      d = a + b;
    end
  end

  always_ff @(posedge clk) begin
    if (reset) begin
      counter = 0;
    end
    else begin
      counter = counter + 1;
    end
  end

endmodule
