module Top;
  int fd;
  int pos_start;
  int pos_after_read;
  int seek_result;
  int pos_after_seek;
  int byte_after_seek;
  int end_seek_result;
  int pos_at_end;
  initial begin
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "0123456789");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    pos_start = $ftell(fd);                 // 0

    void'($fgetc(fd));
    void'($fgetc(fd));
    void'($fgetc(fd));
    pos_after_read = $ftell(fd);            // 3

    // LRM 21.3.5: operation 0 = SEEK_SET, 1 = SEEK_CUR, 2 = SEEK_END.
    seek_result = $fseek(fd, 7, 0);
    pos_after_seek = $ftell(fd);            // 7
    byte_after_seek = $fgetc(fd);           // '7' = 55

    end_seek_result = $fseek(fd, 0, 2);
    pos_at_end = $ftell(fd);                // 10 (just past last byte)
    $fclose(fd);
  end
endmodule
