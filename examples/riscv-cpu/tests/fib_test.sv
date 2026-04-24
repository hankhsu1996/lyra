module fib_test
  import riscv_package::*;
();

  logic clk = 0;
  logic rst_n = 0;

  always #5 clk = ~clk;

  cpu #(.PROGRAM("programs/fibonacci.hex")) dut (
      .clk   (clk),
      .rst_n (rst_n)
  );

  initial begin
    #20 rst_n = 1;
    repeat (100) @(posedge clk);

    if (dut.regfile.registers[3] == 55) begin
      $display("fib_test: PASS (x3 = 55, fib(10))");
    end else begin
      $display("fib_test: FAIL (x3 = %0d, expected 55)", dut.regfile.registers[3]);
      $error("Test failed");
    end

    $finish;
  end

endmodule
