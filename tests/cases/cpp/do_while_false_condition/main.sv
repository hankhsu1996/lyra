module Top;
  initial begin
    int x = 10;
    do begin
      x = 20;
    end while (0);
    $display("x=%0d", x);
  end
endmodule
