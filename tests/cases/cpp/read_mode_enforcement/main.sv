module Top;
  // LRM 21.3.4: "Files opened using file descriptors (fd) can be read from
  // only if they were opened with either the r or r+ type values."
  // (Plus w+, a+ which also include the read permission per Table 21-6.)
  int getc_ret;
  string getc_err_msg;
  int getc_err_code;

  int gets_ret;
  string gets_buf;
  string gets_err_msg;
  int gets_err_code;

  bit [31:0] fread_buf;
  int fread_ret;
  string fread_err_msg;
  int fread_err_code;

  int fscanf_ret;
  int fscanf_dest;
  string fscanf_err_msg;
  int fscanf_err_code;

  // r+ confirms the permits_read path on a mode that also permits writing.
  int rw_first_byte;
  initial begin
    int fd;

    // Setup: file content used by all read-mode tests.
    fd = $fopen("data.txt", "w");
    $fwrite(fd, "ABCD");
    $fclose(fd);

    // Write-only FD must reject $fgetc with EBADF + a "not open for
    // reading" diagnostic, even if the underlying stream would also
    // happen to fail (e.g. on EOF). The discriminating signal is the
    // error message, not the return value.
    fd = $fopen("data.txt", "w");
    getc_ret = $fgetc(fd);
    getc_err_code = $ferror(fd, getc_err_msg);
    $fclose(fd);

    // Same enforcement on $fgets.
    fd = $fopen("data.txt", "w");
    gets_ret = $fgets(gets_buf, fd);
    gets_err_code = $ferror(fd, gets_err_msg);
    $fclose(fd);

    // Same enforcement on $fread.
    fd = $fopen("data.txt", "w");
    fread_ret = $fread(fread_buf, fd);
    fread_err_code = $ferror(fd, fread_err_msg);
    $fclose(fd);

    // Same enforcement on $fscanf.
    fd = $fopen("data.txt", "w");
    fscanf_ret = $fscanf(fd, "%d", fscanf_dest);
    fscanf_err_code = $ferror(fd, fscanf_err_msg);
    $fclose(fd);

    // r+ permits read. Recreate the file (the "w" opens above truncated
    // each time; the final state at this point is empty).
    fd = $fopen("data.txt", "w");
    $fwrite(fd, "X");
    $fclose(fd);
    fd = $fopen("data.txt", "r+");
    rw_first_byte = $fgetc(fd);  // 'X' = 88
    $fclose(fd);
  end
endmodule
