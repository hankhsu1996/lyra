module Top;
  int x;
  initial begin
    $display(x);
    $write(x);
    $fdisplay(1, x);
    $fwrite(1, x);
  end
endmodule
