// A $readmem task treats a packed element as the vector equivalent of its
// bits, so a memory of packed structs loads exactly as a memory of vectors of
// the same width does. Loading a dynamic array or a queue leaves its current
// size alone rather than resizing it, and loading an address of an associative
// array creates an element at that index if none was there (LRM 21.4.1).
module Top;
  typedef struct packed {
    logic [3:0] hi;
    logic [3:0] lo;
  } nibbles;

  nibbles structs[0:2];
  bit [7:0] elements[];
  bit [15:0] items[$];
  bit [7:0] sparse[int];

  int element_count;
  int item_count;
  int sparse_count;
  bit three_exists;

  int fd;

  initial begin
    fd = $fopen("structs.hex", "w");
    $fwrite(fd, "ab cd ef\n");
    $fclose(fd);
    $readmemh("structs.hex", structs);

    elements = new[4];
    fd = $fopen("elements.hex", "w");
    $fwrite(fd, "0a 0b 0c 0d\n");
    $fclose(fd);
    $readmemh("elements.hex", elements);
    element_count = elements.size();

    items = '{16'h0000, 16'h0000, 16'h0000};
    fd = $fopen("items.hex", "w");
    $fwrite(fd, "1111 2222 3333\n");
    $fclose(fd);
    $readmemh("items.hex", items);
    item_count = items.size();

    fd = $fopen("sparse.hex", "w");
    $fwrite(fd, "@2\naa\n@5\nbb\n");
    $fclose(fd);
    $readmemh("sparse.hex", sparse);
    sparse_count = sparse.num();
    three_exists = sparse.exists(3);
  end

  final begin
    if (structs[0].hi !== 4'ha || structs[0].lo !== 4'hb)
      $fatal(1, "structs[0] was %h%h, expected ab", structs[0].hi,
             structs[0].lo);
    if (structs[1].hi !== 4'hc || structs[1].lo !== 4'hd)
      $fatal(1, "structs[1] was %h%h, expected cd", structs[1].hi,
             structs[1].lo);
    if (structs[2].hi !== 4'he || structs[2].lo !== 4'hf)
      $fatal(1, "structs[2] was %h%h, expected ef", structs[2].hi,
             structs[2].lo);

    if (element_count !== 4)
      $fatal(1, "the load resized the dynamic array to %0d, expected 4",
             element_count);
    if (elements[0] !== 8'h0a)
      $fatal(1, "elements[0] was %h, expected 0a", elements[0]);
    if (elements[3] !== 8'h0d)
      $fatal(1, "elements[3] was %h, expected 0d", elements[3]);

    if (item_count !== 3)
      $fatal(1, "the queue held %0d elements after the load, expected 3",
             item_count);
    if (items[0] !== 16'h1111)
      $fatal(1, "items[0] was %h, expected 1111", items[0]);
    if (items[2] !== 16'h3333)
      $fatal(1, "items[2] was %h, expected 3333", items[2]);

    if (sparse_count !== 2)
      $fatal(1, "the associative array held %0d entries, expected 2",
             sparse_count);
    if (sparse[2] !== 8'haa)
      $fatal(1, "sparse[2] was %h, expected aa", sparse[2]);
    if (sparse[5] !== 8'hbb)
      $fatal(1, "sparse[5] was %h, expected bb", sparse[5]);
    if (three_exists !== 1'b0)
      $fatal(1, "the load created an entry at an index the file never named");
    $display("All checks passed");
  end
endmodule
