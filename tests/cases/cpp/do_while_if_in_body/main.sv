module Top;
  initial begin
    int i = 0;
    int odd = 0;
    do begin
      i = i + 1;
      if (i < 4)
        odd = odd + 1;
    end while (i < 5);
    $display("i=%0d odd=%0d", i, odd);
  end
endmodule
