module Top;
  initial begin
    $display("before fatal");
    $fatal;
    $display("after fatal");
  end
endmodule
