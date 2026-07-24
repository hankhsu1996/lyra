module Top;
  // LRM 21.4 error / warning paths. Each case emits a runtime diagnostic and
  // the simulation continues (a missing or malformed load is not fatal).
  bit [7:0] mem_miss [0:3];
  bit [7:0] mem_oor  [0:3];
  bit [7:0] mem_cnt  [0:3];

  int fd;

  initial begin
    // Missing file: error, memory left at default.
    $readmemh("no_such_file.hex", mem_miss);

    // @address outside the memory range: error, load stops before any write.
    fd = $fopen("oor.hex", "w");
    $fwrite(fd, "@9\n0a\n");
    $fclose(fd);
    $readmemh("oor.hex", mem_oor);

    // Explicit range wider than the file's word count, no in-file @address:
    // warning. The two words present still load.
    fd = $fopen("cnt.hex", "w");
    $fwrite(fd, "0a 0b\n");
    $fclose(fd);
    $readmemh("cnt.hex", mem_cnt, 0, 3);
  end
endmodule
