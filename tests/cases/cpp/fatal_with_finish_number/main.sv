module Top;
  initial begin
    $display("before");
    $fatal(2, "code=%0d", 99);
    $display("after");
  end
endmodule
