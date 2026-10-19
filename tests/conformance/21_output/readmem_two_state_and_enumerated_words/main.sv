// @reports: which is no member of the memory's enumeration
//
// A load into a memory of a 2-state type reads an x or z digit as 0. A load
// into a memory of an enumerated type reads the members' numeric values, and a
// number that is no member's value is an error after which nothing more is
// read: the words before it are stored and every address after it keeps what
// it held (LRM 21.4.2). The number is the whole of what the file writes, so a
// word too wide for the type is no member even where its low digits are one,
// and a member holding x or z is read where the word matches it bit for bit.
// The rule holds for every memory a load fills (LRM 21.4.1) and for both
// radixes, and a word the load reads for an address past the memory ends it
// as one inside does.
module Top;
  typedef enum logic [3:0] {A = 1, B = 2, C = 4} four_state_t;
  typedef enum bit [3:0] {P = 0, Q = 3} two_state_t;
  typedef enum int {I0 = 0, I5 = 5} int_based_t;
  typedef enum logic [1:0] {KX = 2'bx1, K0 = 2'b00} unknown_member_t;

  int ints[4];
  bit [3:0] bits[4];
  four_state_t fixed[4];
  four_state_t descending[3:0];
  four_state_t windowed[4];
  four_state_t from_x[3];
  four_state_t too_wide[2];
  two_state_t two_state[5];
  int_based_t int_based[3];
  unknown_member_t unknown_member[2];
  four_state_t dynamic[];
  four_state_t queued[$];
  four_state_t rows[2][2];
  four_state_t sliced[6];
  four_state_t keyed[int];
  four_state_t binary[4];
  four_state_t past_the_memory[2];

  int fd;

  initial begin
    fd = $fopen("unknown_digits.hex", "w");
    $fwrite(fd, "1 x z 3\n");
    $fclose(fd);
    $readmemh("unknown_digits.hex", ints);
    $readmemh("unknown_digits.hex", bits);

    fd = $fopen("no_member.hex", "w");
    $fwrite(fd, "1 2 7 1\n");
    $fclose(fd);
    fixed = '{default: C};
    $readmemh("no_member.hex", fixed);
    descending = '{default: C};
    $readmemh("no_member.hex", descending);
    windowed = '{default: C};
    $readmemh("no_member.hex", windowed, 3, 0);
    dynamic = new[4];
    foreach (dynamic[i]) dynamic[i] = C;
    $readmemh("no_member.hex", dynamic);
    queued = '{C, C, C, C};
    $readmemh("no_member.hex", queued);
    rows = '{default: C};
    $readmemh("no_member.hex", rows);
    sliced = '{default: C};
    $readmemh("no_member.hex", sliced[1:4]);

    fd = $fopen("keyed.hex", "w");
    $fwrite(fd, "@0 1 @1 7 @2 2\n");
    $fclose(fd);
    $readmemh("keyed.hex", keyed);

    fd = $fopen("no_member.bin", "w");
    $fwrite(fd, "0001 0010 0111 0001\n");
    $fclose(fd);
    binary = '{default: C};
    $readmemb("no_member.bin", binary);

    fd = $fopen("past_the_memory.hex", "w");
    $fwrite(fd, "7 2 1\n");
    $fclose(fd);
    past_the_memory = '{default: C};
    $readmemh("past_the_memory.hex", past_the_memory, 2, 0);

    fd = $fopen("from_x.hex", "w");
    $fwrite(fd, "1 x 2\n");
    $fclose(fd);
    from_x = '{default: C};
    $readmemh("from_x.hex", from_x);

    fd = $fopen("too_wide.hex", "w");
    $fwrite(fd, "11 1\n");
    $fclose(fd);
    too_wide = '{default: C};
    $readmemh("too_wide.hex", too_wide);

    fd = $fopen("two_state.hex", "w");
    $fwrite(fd, "3 x 0 5 3\n");
    $fclose(fd);
    two_state = '{default: Q};
    $readmemh("two_state.hex", two_state);

    fd = $fopen("int_based.hex", "w");
    $fwrite(fd, "5 6 0\n");
    $fclose(fd);
    int_based = '{default: I0};
    $readmemh("int_based.hex", int_based);

    fd = $fopen("unknown_member.bin", "w");
    $fwrite(fd, "x1 00\n");
    $fclose(fd);
    unknown_member = '{default: K0};
    $readmemb("unknown_member.bin", unknown_member);
  end

  final begin
    automatic int expected_ints[4] = '{1, 0, 0, 3};
    automatic bit [3:0] expected_bits[4] = '{1, 0, 0, 3};
    automatic four_state_t stopped[4] = '{A, B, C, C};
    automatic four_state_t windowed_stopped[4] = '{C, C, B, A};
    automatic four_state_t rows_stopped[2][2] = '{'{A, B}, '{C, C}};
    automatic four_state_t sliced_stopped[6] = '{C, A, B, C, C, C};
    automatic four_state_t from_x_stopped[3] = '{A, C, C};
    automatic four_state_t too_wide_stopped[2] = '{C, C};
    automatic two_state_t two_state_stopped[5] = '{Q, P, P, Q, Q};
    automatic int_based_t int_based_stopped[3] = '{I5, I0, I0};
    automatic unknown_member_t unknown_member_read[2] = '{KX, K0};
    automatic four_state_t past_stopped[2] = '{C, C};

    if (ints != expected_ints)
      $fatal(1, "an int memory read %p, expected x and z as 0", ints);
    if (bits != expected_bits)
      $fatal(1, "a bit memory read %p, expected x and z as 0", bits);

    if (fixed !== stopped)
      $fatal(1, "a fixed memory read %p, expected %p", fixed, stopped);
    foreach (stopped[i])
      if (descending[i] !== stopped[i])
        $fatal(1, "a descending memory read %p, expected the load to stop at address 2",
               descending);
    if (windowed !== windowed_stopped)
      $fatal(1, "a window counting down read %p, expected %p", windowed,
             windowed_stopped);
    foreach (stopped[i])
      if (dynamic[i] !== stopped[i])
        $fatal(1, "a dynamic array read %p, expected %p", dynamic, stopped);
    foreach (stopped[i])
      if (queued[i] !== stopped[i])
        $fatal(1, "a queue read %p, expected %p", queued, stopped);
    if (rows !== rows_stopped)
      $fatal(1, "a memory of rows read %p, expected %p", rows, rows_stopped);
    if (sliced !== sliced_stopped)
      $fatal(1, "a slice read %p, expected %p", sliced, sliced_stopped);
    if (keyed.num() != 1 || keyed[0] !== A)
      $fatal(1, "an associative memory read %p, expected only its first entry", keyed);
    if (binary !== stopped)
      $fatal(1, "$readmemb read %p, expected %p", binary, stopped);

    if (past_the_memory !== past_stopped)
      $fatal(1, "a word past the memory read %p, expected the load to stop at it",
             past_the_memory);
    if (from_x !== from_x_stopped)
      $fatal(1, "an x word read %p, expected the load to stop at it", from_x);
    if (too_wide !== too_wide_stopped)
      $fatal(1, "a word wider than its type read %p, expected the load to stop at it",
             too_wide);
    if (two_state !== two_state_stopped)
      $fatal(1, "a 2-state enumeration read %p, expected %p", two_state,
             two_state_stopped);
    if (int_based !== int_based_stopped)
      $fatal(1, "an int-based enumeration read %p, expected %p", int_based,
             int_based_stopped);
    if (unknown_member !== unknown_member_read)
      $fatal(1, "a member holding x read %p, expected %p", unknown_member,
             unknown_member_read);
    $display("All checks passed");
  end
endmodule
