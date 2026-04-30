module Top;
  initial begin
    $display("a");
    #0;
    $display("b");
    #0;
    $display("c");
  end
  initial begin
    $display("d");
  end
endmodule
