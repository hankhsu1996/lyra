module Top;
  // LRM 21.3.7: read entries on a closed / unmapped FD stamp EBADF with
  // a descriptive message naming the system task. The setup ($fopen
  // then $fclose) leaves the FD-pool slot allocated but empty, so
  // ResolveSlot returns nullptr -- the path under test for every read
  // entry.
  int getc_ret;
  int getc_errno;
  string getc_msg;

  string gets_buf;
  int gets_ret;
  int gets_errno;
  string gets_msg;

  bit [31:0] fread_buf;
  int fread_ret;
  int fread_errno;
  string fread_msg;

  int fscanf_ret;
  int fscanf_dest;
  int fscanf_errno;
  string fscanf_msg;
  int fscanf_msg_len;
  initial begin
    int fd;

    fd = $fopen("scratch.txt", "w");
    $fclose(fd);
    getc_ret = $fgetc(fd);
    getc_errno = $ferror(fd, getc_msg);

    fd = $fopen("scratch.txt", "w");
    $fclose(fd);
    gets_ret = $fgets(gets_buf, fd);
    gets_errno = $ferror(fd, gets_msg);

    fd = $fopen("scratch.txt", "w");
    $fclose(fd);
    fread_ret = $fread(fread_buf, fd);
    fread_errno = $ferror(fd, fread_msg);

    // copy-in preserves fscanf_dest on a -1 return.
    fscanf_dest = 99;
    fd = $fopen("scratch.txt", "w");
    $fclose(fd);
    fscanf_ret = $fscanf(fd, "%d", fscanf_dest);
    fscanf_errno = $ferror(fd, fscanf_msg);
    fscanf_msg_len = fscanf_msg.len();
  end
endmodule
