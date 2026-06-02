module Top;
  int fd;
  int first;
  int unget_result;
  int second;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "ab");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    first = $fgetc(fd);          // 'a' = 97
    // Push 'Z' (90) back; the next $fgetc must yield it, not 'b'.
    unget_result = $ungetc(90, fd);
    second = $fgetc(fd);
    $fclose(fd);
  end
endmodule
