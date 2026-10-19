// @reports-nothing:
//
// A load into a memory of an enumerated type whose file writes only the
// members' values reads every word and reports nothing (LRM 21.4.2).
module Top;
  typedef enum logic [3:0] {A = 1, B = 2, C = 4} four_state_t;

  four_state_t words[3];

  int fd;

  initial begin
    fd = $fopen("members.hex", "w");
    $fwrite(fd, "1 2 4\n");
    $fclose(fd);
    $readmemh("members.hex", words);
  end

  final begin
    automatic four_state_t expected[3] = '{A, B, C};
    if (words !== expected)
      $fatal(1, "an enumerated memory read %p, expected %p", words, expected);
    $display("All checks passed");
  end
endmodule
