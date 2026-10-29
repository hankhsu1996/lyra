// @argv: +DEST=7
//
// An output or inout actual is read when the subroutine is called and written
// when it returns (LRM 13.5), and the standard does not say how many times an
// expression inside it -- an index, a handle -- is evaluated for the two, as it
// does for a compound assignment's left-hand index (LRM 11.4.1). The same
// silence covers the arguments the language itself reads and writes back: an
// associative array traversal's index (LRM 7.9.4 to 7.9.7), the seed of a
// distribution function or `$random` (LRM 20.14.1, 20.14.2), and the variable
// `$value$plusargs`, `$fread` or `$readmemh` stores into (LRM 21.6, 21.3.4.4,
// 21.4). Lyra evaluates such an expression once, as the source writes it once,
// so the copy out lands in the element the copy in was read from. This case
// holds Lyra to that, through a task, a function, a handle, each of the
// language's own, and a member of a concatenation.
module Top;
  typedef int ints_t [4];
  typedef bit [7:0] bytes_t [4];
  typedef bit [7:0] row_t [0:2];
  typedef logic [3:0] slots_t [4];

  class Holder;
    int field;
  endclass

  ints_t arr;
  int keys [4];
  ints_t seeds;
  int map [int];
  Holder held;
  ints_t converted;
  bytes_t bytes;
  row_t memories [2];
  slots_t slots;
  logic [3:0] beside;
  int fd;
  int matched;
  int bytes_read;
  int drawn;

  int calls;
  int counts [string];

  function automatic int counted(int value);
    calls = calls + 1;
    return value;
  endfunction

  function automatic Holder the_holder();
    calls = calls + 1;
    return held;
  endfunction

  task automatic bump(inout int x);
    x = x + 1;
  endtask

  function automatic void bump_in_function(inout int x);
    x = x + 10;
  endfunction

  task automatic invert(inout logic [7:0] value);
    value = ~value;
  endtask

  initial begin
    held = new;
    arr = '{5, 5, 5, 5};
    map[3] = 30;
    map[7] = 70;
    seeds = '{1, 1, 1, 1};
    converted = '{5, 5, 5, 5};
    slots = '{4'h0, 4'h0, 4'h9, 4'h0};
    beside = 4'h1;

    calls = 0;
    bump(arr[counted(2)]);
    counts["a task's inout actual"] = calls;

    calls = 0;
    bump_in_function(arr[counted(1)]);
    counts["a function's inout actual"] = calls;

    calls = 0;
    held.field = 4;
    bump(the_holder().field);
    counts["an inout property's handle"] = calls;

    calls = 0;
    keys[3] = 3;
    void'(map.next(keys[counted(3)]));
    counts["a traversal's index"] = calls;

    calls = 0;
    drawn = $dist_uniform(seeds[counted(2)], 0, 9);
    counts["a distribution's seed"] = calls;

    calls = 0;
    drawn = $random(seeds[counted(1)]);
    counts["the seed of $random"] = calls;

    calls = 0;
    matched = $value$plusargs("DEST=%d", converted[counted(2)]);
    counts["the variable of $value$plusargs"] = calls;

    fd = $fopen("one_byte.bin", "wb");
    $fwrite(fd, "%c", 8'hA5);
    $fclose(fd);
    fd = $fopen("one_byte.bin", "rb");
    calls = 0;
    bytes_read = $fread(bytes[counted(1)], fd);
    counts["the variable of $fread"] = calls;
    $fclose(fd);

    fd = $fopen("one_row.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    calls = 0;
    $readmemh("one_row.hex", memories[counted(1)]);
    counts["the memory of $readmemh"] = calls;

    calls = 0;
    invert({slots[counted(2)], beside});
    counts["a concatenation member's index"] = calls;
  end

  final begin
    foreach (counts[what])
      if (counts[what] !== 1)
        $fatal(1, "%s was evaluated %0d times, expected 1", what, counts[what]);
    if (counts.num() !== 10)
      $fatal(1, "%0d evaluations were counted, expected 10", counts.num());

    if (arr != ints_t'{5, 15, 6, 5})
      $fatal(1, "the subroutines left arr at %p, expected '{5, 15, 6, 5}", arr);
    if (held.field !== 5)
      $fatal(1, "the property was left at %0d, expected 5", held.field);
    if (keys[3] !== 7)
      $fatal(1, "the traversal left its index at %0d, expected 7", keys[3]);
    if (seeds[0] !== 1 || seeds[1] === 1 || seeds[2] === 1 || seeds[3] !== 1)
      $fatal(1, "the draws left the seeds at %p", seeds);
    if (matched === 0) $fatal(1, "DEST=%%d did not match +DEST=7");
    if (converted != ints_t'{5, 5, 7, 5})
      $fatal(1, "the plusarg left converted at %p, expected '{5, 5, 7, 5}",
             converted);
    if (bytes_read !== 1 || bytes != bytes_t'{8'h00, 8'hA5, 8'h00, 8'h00})
      $fatal(1, "the read returned %0d and left bytes at %p", bytes_read,
             bytes);
    if (memories[1] != row_t'{8'h0a, 8'h0b, 8'h0c} ||
        memories[0] != row_t'{8'h00, 8'h00, 8'h00})
      $fatal(1, "the load left memories at %p", memories);
    if (slots != slots_t'{4'h0, 4'h0, 4'h6, 4'h0} || beside !== 4'hE)
      $fatal(1, "the concatenation left slots at %p and beside at %h", slots,
             beside);
    $display("All checks passed");
  end
endmodule
