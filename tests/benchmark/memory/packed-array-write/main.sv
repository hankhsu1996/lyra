// @measure: run
// @work: array pass
//
// One pass writes every element of a packed array declared with two dimensions
// and indexed element by element. The checksum afterwards is what stops the
// stores being deleted as unread.
module Top;
  localparam int ARRAY_SIZE = 1024;

  logic [ARRAY_SIZE-1:0][31:0] data;
  int sum;

  initial begin
    int num_passes;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    data = '0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) data[i] = 32'(i * 3 + pass);
    end

    sum = 0;
    for (int i = 0; i < ARRAY_SIZE; i++) sum = sum + int'(data[i]);

    $display("packed-array-write done: sum=%0d", sum);
    $finish;
  end
endmodule
