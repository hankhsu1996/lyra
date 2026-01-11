module all_tests
  import riscv_package::*;
();

  logic clk = 0;
  logic rst_n = 0;
  int pass_count = 0;
  int fail_count = 0;

  always #5 clk = ~clk;

  cpu #(.PROGRAM("programs/sum_1_to_10.hex")) sum_cpu (
      .clk   (clk),
      .rst_n (rst_n)
  );

  cpu #(.PROGRAM("programs/fibonacci.hex")) fib_cpu (
      .clk   (clk),
      .rst_n (rst_n)
  );

  initial begin
    $display("Running all tests...");
    $display("");

    #20 rst_n = 1;
    repeat (100) @(posedge clk);

    // Check sum test
    if (sum_cpu.regfile.registers[3] == 55) begin
      $display("sum_test: PASS (x3 = 55)");
      pass_count++;
    end else begin
      $display("sum_test: FAIL (x3 = %0d, expected 55)", sum_cpu.regfile.registers[3]);
      fail_count++;
    end

    // Check fibonacci test
    if (fib_cpu.regfile.registers[3] == 55) begin
      $display("fib_test: PASS (x3 = 55, fib(10))");
      pass_count++;
    end else begin
      $display("fib_test: FAIL (x3 = %0d, expected 55)", fib_cpu.regfile.registers[3]);
      fail_count++;
    end

    $display("");
    $display("Results: %0d passed, %0d failed", pass_count, fail_count);

    $finish;
  end

endmodule
