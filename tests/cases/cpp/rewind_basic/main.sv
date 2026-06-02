module Top;
  int fd;
  int first_byte;
  int rewind_result;
  int first_byte_again;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "rocket");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    first_byte = $fgetc(fd);              // 'r' = 114
    rewind_result = $rewind(fd);          // LRM 21.3.5: same as fseek(fd, 0, 0)
    first_byte_again = $fgetc(fd);        // 'r' again
    $fclose(fd);
  end
endmodule
