// $fread loads a memory from the address start names, and "start is the
// address in the memory" (LRM 21.3.4.4). Where the memory is a slice of an
// array, the standard does not say whether that address is an index of the
// array or a position in the slice. Lyra takes it as an index of the array,
// one the slice holds, as $readmemh does for a slice (LRM 21.4), for a
// fixed-size and a dynamic array alike. This case holds Lyra to that.
module Top;
  int fd;
  bit [31:0] sliced[0:7];
  bit [31:0] sliced_expected[0:7] =
      '{0, 0, 0, 0, 32'h01020304, 32'h05060708, 0, 0};
  bit [31:0] dynamic_sliced[];
  int dynamic_sliced_count;
  int sliced_count;

  initial begin
    fd = $fopen("sixteen.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'h01, 8'h02, 8'h03, 8'h04, 8'h05, 8'h06, 8'h07, 8'h08,
            8'h09, 8'h0A, 8'h0B, 8'h0C, 8'h0D, 8'h0E, 8'h0F, 8'h10);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    sliced_count = $fread(sliced[2:5], fd, 4);
    $fclose(fd);

    dynamic_sliced = new[8];
    fd = $fopen("sixteen.bin", "rb");
    dynamic_sliced_count = $fread(dynamic_sliced[2:5], fd, 4);
    $fclose(fd);
  end

  final begin
    if (sliced_count !== 8)
      $fatal(1, "two words of a slice returned %0d, expected 8", sliced_count);
    if (sliced !== sliced_expected)
      $fatal(1, "a slice loaded from index 4 left %p, expected its words at 4 and 5",
             sliced);
    if (dynamic_sliced_count !== 8)
      $fatal(1, "two words of a slice of a dynamic array returned %0d, expected 8",
             dynamic_sliced_count);
    if (dynamic_sliced[4] !== 32'h01020304 || dynamic_sliced[5] !== 32'h05060708 ||
        dynamic_sliced[3] !== 32'h0 || dynamic_sliced[6] !== 32'h0)
      $fatal(1, "a slice of a dynamic array loaded from index 4 left %p", dynamic_sliced);
    $display("All checks passed");
  end
endmodule
