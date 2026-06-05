module Top;
  // LRM 21.3.4.4 memory form. Five SV call shapes; one element type per
  // sub-scenario to cover the variation that matters: full fill, non-zero
  // base, explicit start+count, descending range with comma-elision, odd
  // element width, EOF mid-element.
  bit [31:0] form_a [0:3];
  int n_a;
  bit [31:0] form_b [10:13];
  int n_b;
  bit [31:0] form_c [0:3];
  int n_c;
  bit [31:0] form_d [20:17];
  int n_d;
  bit [8:0] form_w [0:2];
  int n_w;
  bit [31:0] form_partial [0:3];
  int n_partial;
  initial begin
    int fd;

    fd = $fopen("a.bin", "wb");
    $fwrite(
        fd, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c", 8'h01, 8'h02, 8'h03, 8'h04,
        8'h05, 8'h06, 8'h07, 8'h08, 8'h09, 8'h0A, 8'h0B, 8'h0C, 8'h0D, 8'h0E,
        8'h0F, 8'h10);
    $fclose(fd);

    // Form 2a: $fread(mem, fd). Default start=0, default count=size.
    fd = $fopen("a.bin", "rb");
    n_a = $fread(form_a, fd);
    $fclose(fd);

    // Same content, declared range [10:13] (non-zero ascending base).
    fd = $fopen("a.bin", "rb");
    n_b = $fread(form_b, fd);
    $fclose(fd);

    // Form 2c: explicit start=1, count=2. Fills SV indices 1 and 2; indices
    // 0 and 3 stay at default-init zero.
    fd = $fopen("a.bin", "rb");
    n_c = $fread(form_c, fd, 1, 2);
    $fclose(fd);

    // Form 2d: descending range [20:17], comma-elision (default start,
    // explicit count=2). Loading goes "upward toward highest numerical
    // address" (LRM 21.3.4.4): from SV index 17, then 18. SV indices 19
    // and 20 stay at default-init zero.
    fd = $fopen("a.bin", "rb");
    n_d = $fread(form_d, fd, , 2);
    $fclose(fd);

    // Odd element width (9 bits). Two bytes per element; the top 9 bits
    // of each 16-bit group fill one element MSB-first (LRM "first byte
    // read is used to fill the most significant location").
    fd = $fopen("w.bin", "wb");
    $fwrite(fd, "%c%c%c%c%c%c", 8'h80, 8'h00, 8'hC0, 8'h00, 8'h7F, 8'h80);
    $fclose(fd);
    fd = $fopen("w.bin", "rb");
    n_w = $fread(form_w, fd);
    $fclose(fd);

    // EOF mid-element. File has 7 bytes; element 0 (4 bytes) fills
    // completely; element 1 gets 3 bytes with the LSB byte zero-padded
    // (matching D-1 integral form's "as much as available" semantic).
    // Elements 2 and 3 stay at default-init zero.
    fd = $fopen("partial.bin", "wb");
    $fwrite(
        fd, "%c%c%c%c%c%c%c", 8'hAA, 8'hBB, 8'hCC, 8'hDD, 8'hEE, 8'hFF, 8'h11);
    $fclose(fd);
    fd = $fopen("partial.bin", "rb");
    n_partial = $fread(form_partial, fd);
    $fclose(fd);
  end
endmodule
