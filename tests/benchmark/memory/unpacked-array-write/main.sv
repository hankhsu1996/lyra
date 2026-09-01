// @measure: run
// @work: array pass
//
// One pass writes every element of a 32768-element unpacked array, so what a
// pass costs is the store path and the index computation behind it. The
// array's length is the case's subject rather than its size: while a partial
// write materializes the whole value, what one element costs is a function of
// how many elements there are.
module Top;
  localparam int ARRAY_SIZE = 32768;

  logic [31:0] data [0:ARRAY_SIZE-1];

  initial begin
    int num_passes;
    int checksum;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) data[i] = i * 3 + pass;
    end

    checksum = 0;
    for (int i = 0; i < ARRAY_SIZE; i++) checksum = checksum + int'(data[i]);

    $display("unpacked-array-write done: checksum=%0d", checksum);
    $finish;
  end
endmodule
