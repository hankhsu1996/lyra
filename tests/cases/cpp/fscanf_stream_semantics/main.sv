module Top;
  // LRM 21.3.4.3 source-driven semantics for $fscanf. The format/conversion
  // semantics themselves are covered by the sscanf tests; this case targets
  // the file-source-specific surface:
  //   - EOF before any conversion -> -1
  //   - partial match returns the matched count
  //   - literal mismatch: scan stops, offending byte is left unread
  //     (verified by a subsequent $fgetc returning that exact byte)
  //   - $feof reflects EOF after a scan that drained the file
  //   - $ungetc'd byte is visible to $fscanf (proves scanner uses the
  //     underlying stream rather than bypassing the FD's putback buffer)
  //   - assign-RHS form
  int fd;

  int code_empty, a_empty;

  int code_partial, a_partial, b_partial;

  int code_lit, a_lit, b_lit;
  int leftover_byte;

  int code_drain;
  int a_drain;
  int eof_after_drain;
  int peek_past_eof;

  int code_ungetc, ret_ungetc;
  int first_byte, c_ungetc;

  int code_rhs, a_rhs, b_rhs;

  initial begin
    // Empty file: EOF before any conversion -> -1.
    fd = $fopen("empty.txt", "w");
    $fclose(fd);
    a_empty = 99;
    fd = $fopen("empty.txt", "r");
    code_empty = $fscanf(fd, "%d", a_empty);
    $fclose(fd);

    // Partial match: second %d sees 'a' (not digit), mismatch -> return 1.
    b_partial = 99;
    fd = $fopen("partial.txt", "w");
    $fwrite(fd, "12 abc");
    $fclose(fd);
    fd = $fopen("partial.txt", "r");
    code_partial = $fscanf(fd, "%d %d", a_partial, b_partial);
    $fclose(fd);

    // Literal mismatch: ':' in format does not match 'x' in input. After
    // the scan stops, the next $fgetc must observe 'x' (LRM 21.3.4.3
    // "the offending input character is left unread in the input stream").
    b_lit = 99;
    fd = $fopen("lit.txt", "w");
    $fwrite(fd, "12x34");
    $fclose(fd);
    fd = $fopen("lit.txt", "r");
    code_lit = $fscanf(fd, "%d:%d", a_lit, b_lit);
    leftover_byte = $fgetc(fd);
    $fclose(fd);

    // Drain to EOF: after scanning everything, $feof returns nonzero.
    fd = $fopen("drain.txt", "w");
    $fwrite(fd, "777");
    $fclose(fd);
    fd = $fopen("drain.txt", "r");
    code_drain = $fscanf(fd, "%d", a_drain);
    // One extra peek to actually push the stream past the last byte; $fscanf
    // by itself stops at the (logical) end without setting fstream's eof
    // bit unless the scanner needed to look past the final digit.
    peek_past_eof = $fgetc(fd);
    eof_after_drain = $feof(fd);
    $fclose(fd);

    // Read one byte, $ungetc it, then $fscanf %c. The scanner must observe
    // the putback'd byte rather than skip past it -- proves $fscanf goes
    // through the underlying FD's input stream (including its putback
    // buffer) rather than bypassing $ungetc state. Putback of the
    // most-recently-read byte is the portable form per std::istream.
    fd = $fopen("ung.txt", "w");
    $fwrite(fd, "AB");
    $fclose(fd);
    fd = $fopen("ung.txt", "r");
    first_byte = $fgetc(fd);                       // 'A' = 0x41 = 65
    ret_ungetc = $ungetc(first_byte, fd);          // push 'A' back
    code_ungetc = $fscanf(fd, "%c", c_ungetc);     // expect c == 'A'
    $fclose(fd);

    // Assign-RHS form: code = $fscanf(...).
    fd = $fopen("rhs.txt", "w");
    $fwrite(fd, "5 9");
    $fclose(fd);
    fd = $fopen("rhs.txt", "r");
    code_rhs = $fscanf(fd, "%d %d", a_rhs, b_rhs);
    $fclose(fd);
  end
endmodule
