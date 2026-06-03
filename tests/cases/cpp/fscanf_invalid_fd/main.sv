module Top;
  // LRM 21.3.4.3 + 21.3.7: $fscanf on a closed FD returns -1 (no input
  // available) and stamps $ferror with EBADF + a message naming "$fscanf",
  // so a follow-up call surfaces the error condition.
  int    fd;
  int    code;
  int    a;
  int    errno_val;
  string err_msg;
  int    err_msg_len;
  initial begin
    // Open then close so the FD's pool slot exists (the SetError surface
    // only stamps for allocated FD-pool indexes; a never-opened FD has
    // no slot to stamp into and silently no-ops).
    fd = $fopen("dummy.txt", "w");
    $fclose(fd);

    // Now use the same FD: Resolve returns nullptr (slot is empty after
    // close) so $fscanf hits the invalid-FD short-circuit.
    a = 99;                            // copy-in preserves this on -1 return
    code = $fscanf(fd, "%d", a);
    errno_val = $ferror(fd, err_msg);
    err_msg_len = err_msg.len();
  end
endmodule
