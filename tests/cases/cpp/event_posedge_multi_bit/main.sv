module Top;
  // LRM 9.4.2: edge events observe the LSB of the expression only.
  logic [7:0] bus;
  int         done;
  initial begin
    bus = 8'h00;
    done = 0;
    @(posedge bus);
    done = 1;
  end
  initial begin
    #5;
    // Upper bits change but LSB stays 0; posedge must NOT fire.
    bus = 8'hF0;
    #5;
    // LSB transitions 0->1; posedge fires.
    bus = 8'hF1;
  end
endmodule
