// @argv: +DEST=7
//
// An `inout` argument is copied in when the subroutine is called and copied
// out when it returns (LRM 13.5), so the copy out lands in the element, the
// property or the variable the actual names. The same holds where the
// language itself takes an argument that way: the index an associative
// array's traversal method reads and updates (LRM 7.9.4 to 7.9.7) and the
// seed a distribution function or `$random` reads and advances (LRM 20.14.1,
// 20.14.2). It holds too for a variable a system function is handed to store
// into: the one `$value$plusargs` converts a plusarg into (LRM 21.6), the one
// `$fread` reads bytes into (LRM 21.3.4.4), and the memory `$readmemh` loads,
// which may be a multidimensional array indexed down to a lesser-dimensioned
// one (LRM 21.4).
//
// The value copied in is the actual's value at the formal's type. A member of
// a packed structure keeps its own declared signedness and state domain when
// read (LRM 7.2.1, 11.8.1), so a signed member copies in negative, and a
// 2-state member of a 4-state structure holding x copies in 0.
module Top;
  typedef struct packed {
    logic signed [3:0] s;
    bit [3:0] two;
  } fields_t;
  fields_t fields;
  int seen_signed;
  int seen_two_state;

  task automatic look_signed(inout logic signed [3:0] x);
    seen_signed = x;
    x = x - 1;
  endtask

  task automatic look_two_state(inout bit [3:0] x);
    seen_two_state = x;
    x = x + 1;
  endtask

  class Holder;
    int field;
  endclass

  int arr [4];
  int keys [4];
  int seeds [4];
  int map [int];
  Holder held;

  int converted [4];
  bit [7:0] bytes [4];
  bit [7:0] memories [2][0:2];
  int fd;
  int matched;
  int bytes_read;
  int drawn;

  task automatic bump(inout int x);
    x = x + 1;
  endtask

  function automatic void bump_in_function(inout int x);
    x = x + 10;
  endfunction

  initial begin
    held = new;
    arr = '{5, 5, 5, 5};
    map[3] = 30;
    map[7] = 70;
    seeds = '{1, 1, 1, 1};

    bump(arr[2]);
    bump_in_function(arr[1]);
    held.field = 4;
    bump(held.field);
    keys[3] = 3;
    void'(map.next(keys[3]));
    drawn = $dist_uniform(seeds[2], 0, 9);
    drawn = $random(seeds[1]);

    converted = '{5, 5, 5, 5};
    matched = $value$plusargs("DEST=%d", converted[2]);

    fd = $fopen("one_byte.bin", "wb");
    $fwrite(fd, "%c", 8'hA5);
    $fclose(fd);
    fd = $fopen("one_byte.bin", "rb");
    bytes_read = $fread(bytes[1], fd);
    $fclose(fd);

    fd = $fopen("one_row.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    $readmemh("one_row.hex", memories[1]);

    fields = 'x;
    fields.s = -3;
    look_signed(fields.s);
    look_two_state(fields.two);
  end

  final begin
    if (arr[2] !== 6) $fatal(1, "the task left arr[2] at %0d, expected 6", arr[2]);
    if (arr[1] !== 15)
      $fatal(1, "the function left arr[1] at %0d, expected 15", arr[1]);
    if (arr[0] !== 5 || arr[3] !== 5)
      $fatal(1, "a subroutine wrote an element it was not given");
    if (held.field !== 5)
      $fatal(1, "the property was left at %0d, expected 5", held.field);
    if (keys[3] !== 7)
      $fatal(1, "the traversal left its index at %0d, expected 7", keys[3]);
    if (seeds[2] === 1) $fatal(1, "the draw did not advance its seed");
    if (seeds[1] === 1) $fatal(1, "$random did not advance its seed");
    if (seeds[0] !== 1 || seeds[3] !== 1)
      $fatal(1, "a draw advanced a seed it was not given");

    if (matched === 0) $fatal(1, "DEST=%%d did not match +DEST=7");
    if (converted[2] !== 7)
      $fatal(1, "the plusarg left converted[2] at %0d, expected 7",
             converted[2]);
    if (converted[0] !== 5 || converted[1] !== 5 || converted[3] !== 5)
      $fatal(1, "the plusarg was stored into an element it was not given");
    if (bytes_read !== 1)
      $fatal(1, "reading one byte returned %0d, expected 1", bytes_read);
    if (bytes[1] !== 8'hA5)
      $fatal(1, "the read left bytes[1] at %h, expected a5", bytes[1]);
    if (bytes[0] !== 8'h00 || bytes[2] !== 8'h00 || bytes[3] !== 8'h00)
      $fatal(1, "the read stored into an element it was not given");
    if (memories[1][0] !== 8'h0a || memories[1][1] !== 8'h0b ||
        memories[1][2] !== 8'h0c)
      $fatal(1, "the load left memories[1] at %h %h %h, expected 0a 0b 0c",
             memories[1][0], memories[1][1], memories[1][2]);
    if (memories[0][0] !== 8'h00 || memories[0][2] !== 8'h00)
      $fatal(1, "the load stored into a memory it was not given");

    if (seen_signed !== -3)
      $fatal(1, "a signed member copied in %0d, expected -3", seen_signed);
    if (fields.s !== -4)
      $fatal(1, "a signed member was left at %0d, expected -4", fields.s);
    if (seen_two_state !== 0)
      $fatal(1, "a 2-state member holding x copied in %0d, expected 0",
             seen_two_state);
    if (fields.two !== 1)
      $fatal(1, "a 2-state member was left at %0d, expected 1", fields.two);
    $display("All checks passed");
  end
endmodule
