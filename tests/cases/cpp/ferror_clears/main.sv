module Top;
  int fd;
  int seek_result;
  int err_code;
  string err_msg;
  int err_msg_len;
  int err_code_again;
  string err_msg_again;
  initial begin
    // Create a file then trigger a deterministic error: $fseek with an
    // invalid operation (anything outside 0/1/2). The runtime stamps EINVAL
    // on the FD; $ferror observes it and clears the slot, so a second call
    // sees no error.
    fd = $fopen("scratch.txt", "w");
    $fwrite(fd, "data");
    $fclose(fd);

    fd = $fopen("scratch.txt", "r");
    seek_result = $fseek(fd, 0, 99);
    err_code = $ferror(fd, err_msg);
    err_msg_len = err_msg.len();
    err_code_again = $ferror(fd, err_msg_again);
    $fclose(fd);
  end
endmodule
