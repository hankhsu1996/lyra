// $fread into a memory loads consecutive words toward the highest address of
// the memory, beginning at the lowest numbered location unless a start address
// says otherwise, and stops when the memory is full or the file is spent
// unless a count says otherwise. Each word takes as many whole bytes as it
// spans -- one for an 8-bit word, two for a 9-bit one -- and locations the
// load does not reach keep the value they had (LRM 21.3.4.4). A word is any
// integral type (LRM 6.11.1), so a memory of enumerations or of packed
// structures loads exactly as one of vectors of the same width does.
module Top;
  typedef enum bit [31:0] {
    NONE = 32'h00000000,
    FIRST = 32'h01020304,
    SECOND = 32'h05060708
  } word_t;
  typedef struct packed {
    bit [15:0] upper;
    bit [15:0] lower;
  } halves_t;

  int fd;

  bit [31:0] ascending[0:3];
  int ascending_count;

  bit [31:0] based_at_ten[10:13];
  int based_at_ten_count;

  bit [31:0] windowed[0:3];
  int windowed_count;

  bit [31:0] descending[20:17];
  int descending_count;

  bit [8:0] nine_bit[0:2];
  int nine_bit_count;

  bit [31:0] short_file[0:3];
  int short_file_count;

  word_t enumerated[0:2];
  int enumerated_count;

  halves_t structured[0:1];
  int structured_count;

  initial begin
    fd = $fopen("sixteen.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
            8'h01, 8'h02, 8'h03, 8'h04, 8'h05, 8'h06, 8'h07, 8'h08,
            8'h09, 8'h0A, 8'h0B, 8'h0C, 8'h0D, 8'h0E, 8'h0F, 8'h10);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    ascending_count = $fread(ascending, fd);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    based_at_ten_count = $fread(based_at_ten, fd);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    windowed_count = $fread(windowed, fd, 1, 2);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    descending_count = $fread(descending, fd, , 2);
    $fclose(fd);

    fd = $fopen("nine.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c", 8'h80, 8'h00, 8'hC0, 8'h00, 8'h7F, 8'h80);
    $fclose(fd);
    fd = $fopen("nine.bin", "rb");
    nine_bit_count = $fread(nine_bit, fd);
    $fclose(fd);

    fd = $fopen("seven.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c%c",
            8'hAA, 8'hBB, 8'hCC, 8'hDD, 8'hEE, 8'hFF, 8'h11);
    $fclose(fd);
    fd = $fopen("seven.bin", "rb");
    short_file_count = $fread(short_file, fd);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    enumerated_count = $fread(enumerated, fd, , 2);
    $fclose(fd);

    fd = $fopen("sixteen.bin", "rb");
    structured_count = $fread(structured, fd);
    $fclose(fd);
  end

  final begin
    if (ascending_count !== 16)
      $fatal(1, "filling four 32-bit words returned %0d, expected 16",
             ascending_count);
    if (ascending[0] !== 32'h01020304)
      $fatal(1, "ascending[0] was %h, expected 01020304", ascending[0]);
    if (ascending[3] !== 32'h0D0E0F10)
      $fatal(1, "ascending[3] was %h, expected 0d0e0f10", ascending[3]);

    if (based_at_ten_count !== 16)
      $fatal(1, "filling a memory based at ten returned %0d, expected 16",
             based_at_ten_count);
    if (based_at_ten[10] !== 32'h01020304)
      $fatal(1, "based_at_ten[10] was %h, expected 01020304",
             based_at_ten[10]);
    if (based_at_ten[13] !== 32'h0D0E0F10)
      $fatal(1, "based_at_ten[13] was %h, expected 0d0e0f10",
             based_at_ten[13]);

    if (windowed_count !== 8)
      $fatal(1, "loading two words returned %0d, expected 8", windowed_count);
    if (windowed[0] !== 32'h0)
      $fatal(1, "windowed[0] was %h, expected the value it started with",
             windowed[0]);
    if (windowed[1] !== 32'h01020304)
      $fatal(1, "windowed[1] was %h, expected 01020304", windowed[1]);
    if (windowed[2] !== 32'h05060708)
      $fatal(1, "windowed[2] was %h, expected 05060708", windowed[2]);
    if (windowed[3] !== 32'h0)
      $fatal(1, "windowed[3] was %h, expected the value it started with",
             windowed[3]);

    if (descending_count !== 8)
      $fatal(1, "two words of a descending memory returned %0d, expected 8",
             descending_count);
    if (descending[17] !== 32'h01020304)
      $fatal(1, "descending[17] was %h, expected 01020304", descending[17]);
    if (descending[18] !== 32'h05060708)
      $fatal(1, "descending[18] was %h, expected 05060708", descending[18]);
    if (descending[19] !== 32'h0)
      $fatal(1, "descending[19] was %h, expected the value it started with",
             descending[19]);

    if (nine_bit_count !== 6)
      $fatal(1, "filling three 9-bit words returned %0d, expected 6",
             nine_bit_count);
    if (nine_bit[0] !== 9'h100)
      $fatal(1, "nine_bit[0] was %h, expected 100", nine_bit[0]);
    if (nine_bit[1] !== 9'h180)
      $fatal(1, "nine_bit[1] was %h, expected 180", nine_bit[1]);
    if (nine_bit[2] !== 9'h0FF)
      $fatal(1, "nine_bit[2] was %h, expected 0ff", nine_bit[2]);

    if (short_file_count !== 7)
      $fatal(1, "a seven-byte file returned %0d, expected 7",
             short_file_count);
    if (short_file[0] !== 32'hAABBCCDD)
      $fatal(1, "short_file[0] was %h, expected aabbccdd", short_file[0]);
    if (short_file[3] !== 32'h0)
      $fatal(1, "short_file[3] was %h, expected the value it started with",
             short_file[3]);

    if (enumerated_count !== 8)
      $fatal(1, "loading two enumerated words returned %0d, expected 8",
             enumerated_count);
    if (enumerated[0] !== FIRST)
      $fatal(1, "enumerated[0] was %h, expected 01020304", enumerated[0]);
    if (enumerated[1] !== SECOND)
      $fatal(1, "enumerated[1] was %h, expected 05060708", enumerated[1]);
    if (enumerated[2] !== NONE)
      $fatal(1, "enumerated[2] was %h, expected the value it started with",
             enumerated[2]);

    if (structured_count !== 8)
      $fatal(1, "filling two structured words returned %0d, expected 8",
             structured_count);
    if (structured[0].upper !== 16'h0102)
      $fatal(1, "structured[0].upper was %h, expected 0102",
             structured[0].upper);
    if (structured[0].lower !== 16'h0304)
      $fatal(1, "structured[0].lower was %h, expected 0304",
             structured[0].lower);
    if (structured[1].upper !== 16'h0506)
      $fatal(1, "structured[1].upper was %h, expected 0506",
             structured[1].upper);
    $display("All checks passed");
  end
endmodule
