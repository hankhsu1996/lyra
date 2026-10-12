// A fixed-size unpacked array is its elements whatever holds it and however
// many it is (LRM 7.4.2): a variable, a member of a structure, an element of
// another array or of a queue, a subroutine's argument, local and result, and
// a net. An element is read and written where it lies, an index naming no
// element, constant or not, reads the element type's default and writes
// nothing (LRM 7.4.5), a
// whole assignment copies every element (LRM 7.6), and a reference to an
// element or to a row goes on naming it across a whole assignment (LRM
// 13.5.2). An element may itself be an array (LRM 7.4.4), however large, and
// is then read, written, compared, assigned and passed as any array is.
module Top;
  typedef struct {
    int tag;
    string name;
  } labelled_t;

  typedef struct {
    logic [7:0] head;
    int body[3];
    labelled_t labels[2];
  } record_t;

  typedef int row_t[4];

  int few[4];
  int many[600];
  int many_copy[600];
  int grid[3][4];
  int tall[40][20];
  int deep[2][600];
  int deep_copy[2][600];
  int deep_row[600];
  string words[3];
  int queues[2][$];
  labelled_t labelled[2];
  record_t record;
  record_t records[2];
  row_t rows_in_queue[$];
  logic [99:0] wide[3];
  logic [3:0] four_state[2];
  wire [7:0] wired[2];

  int bad_read;
  int bad_constant_read;
  logic [3:0] bad_read_four_state;
  string bad_read_word;
  int bad_row_read;
  int index;
  logic [1:0] unknown_index;

  int through_element;
  int through_row;
  int sum_few;
  int sum_many;
  row_t turned;
  int from_result;
  int local_kept;
  bit deep_equal_to_its_copy;
  bit deep_equal_after_a_write;
  bit deep_rows_equal;
  int deep_bad_row_read;
  int deep_by_value;
  int deep_after_by_value;
  int deep_through_reference;

  assign wired[0] = 8'h12;
  assign wired[1] = 8'h34;

  function automatic int sum_of(int values[4]);
    int total = 0;
    foreach (values[i]) total += values[i];
    return total;
  endfunction

  function automatic int sum_of_many(int values[600]);
    int local_values[600];
    local_values = values;
    local_values[599] += 1;
    return local_values[0] + local_values[599];
  endfunction

  function automatic row_t reversed(row_t given);
    row_t result;
    foreach (given[i]) result[3 - i] = given[i];
    return result;
  endfunction

  task automatic bump_element(ref int element, input int by);
    few = '{100, 200, 300, 400};
    element += by;
  endtask

  task automatic bump_row(ref int row[4]);
    grid = '{default: 7};
    row[2] += 1;
  endtask

  function automatic int corner_sum(int rows[2][600]);
    rows[1][599] += 1000;
    return rows[0][0] + rows[1][599];
  endfunction

  task automatic bump_last(ref int rows[2][600]);
    deep = deep_copy;
    rows[1][599] += 1;
  endtask

  task automatic bump_row_end(ref int row[600]);
    row[599] += 10;
  endtask

  task automatic keeps_a_local(output int kept);
    int scratch[600];
    scratch[300] = 5;
    #1;
    scratch[301] = scratch[300] + 1;
    kept = scratch[300] + scratch[301] + scratch[0];
  endtask

  initial begin
    few = '{1, 2, 3, 4};
    foreach (many[i]) many[i] = i;
    many_copy = many;
    many[10] = -1;

    grid = '{default: 0};
    grid[2][3] = 9;
    grid[0] = '{1, 2, 3, 4};
    foreach (tall[i, j]) tall[i][j] = i * 100 + j;

    words = '{"a", "bb", "ccc"};
    words[1] = {words[1], words[2]};
    queues[1].push_back(4);
    queues[1].push_back(5);
    queues[0] = queues[1];
    queues[0][0] = 40;

    labelled[1].name = "second";
    labelled[1].tag = 2;
    record.head = 8'hA5;
    record.body = '{1, 2, 3};
    record.body[1] = 20;
    record.labels[1].name = "inner";
    records[1] = record;
    records[1].body[2] = 30;
    records[0].labels[0].name = "first";

    rows_in_queue.push_back('{1, 2, 3, 4});
    rows_in_queue.push_back(reversed(rows_in_queue[0]));
    rows_in_queue[1][0] = 44;

    wide[1] = 100'd5;
    wide[2] = wide[1] << 70;
    four_state[0] = 4'b1010;

    index = 4;
    bad_read = few[index];
    bad_constant_read = 5;
    bad_constant_read = few[4];
    many[600] = 77;
    index = -1;
    few[index] = 77;
    many[index] = 77;
    index = 600;
    many[index] = 77;
    bad_read_four_state = four_state[index];
    bad_read_word = words[index];
    index = 3;
    bad_row_read = grid[index][0];
    grid[index] = '{9, 9, 9, 9};
    unknown_index = 2'bx1;
    few[unknown_index] = 55;

    sum_few = sum_of(few);
    sum_many = sum_of_many(many);
    turned = reversed(few);
    from_result = turned[0];
    keeps_a_local(local_kept);

    bump_element(few[2], 5);
    through_element = few[2];
    bump_row(grid[1]);
    through_row = grid[1][2];

    foreach (deep[i, j]) deep[i][j] = i * 1000 + j;
    deep_copy = deep;
    deep_equal_to_its_copy = (deep == deep_copy);
    deep[1][599] = -1;
    deep_equal_after_a_write = 1'b1;
    deep_equal_after_a_write = (deep == deep_copy);
    deep[0] = deep[1];
    deep_row = deep[0];
    deep_rows_equal = (deep[0] == deep[1]) && (deep_row == deep[1]);
    index = 2;
    deep_bad_row_read = 5;
    deep_bad_row_read = deep[index][3];
    deep[index] = deep_row;
    deep[index][0] = 77;
    index = 600;
    deep[1][index] = 77;
    deep_by_value = corner_sum(deep);
    deep_after_by_value = deep[1][599];
    bump_last(deep);
    deep_through_reference = deep[1][599];
    bump_row_end(deep[0]);
    #1;
  end

  final begin
    if (few[0] !== 100 || few[1] !== 200 || few[3] !== 400)
      $fatal(1, "few was %p", few);
    if (through_element !== 305)
      $fatal(1, "an element's reference read %0d, expected 305",
             through_element);
    if (through_row !== 8 || grid[1][1] !== 7 || grid[0][2] !== 7)
      $fatal(1, "a row's reference left grid as %p", grid);
    if (many[10] !== -1 || many[599] !== 599 || many[0] !== 0)
      $fatal(1, "many held %0d %0d %0d", many[10], many[599], many[0]);
    if (many_copy[10] !== 10 || many_copy[599] !== 599)
      $fatal(1, "many_copy held %0d %0d", many_copy[10], many_copy[599]);
    if (many == many_copy)
      $fatal(1, "two arrays differing in one element compared equal");
    if (tall[39][19] !== 3919 || tall[17][3] !== 1703)
      $fatal(1, "tall held %0d %0d", tall[39][19], tall[17][3]);
    if (words[0] != "a" || words[1] != "bbccc" || words[2] != "ccc")
      $fatal(1, "words was %p", words);
    if (queues[0].size() !== 2 || queues[0][0] !== 40 || queues[1][0] !== 4)
      $fatal(1, "queues was %p", queues);
    if (labelled[1].name != "second" || labelled[0].name != "" ||
        labelled[1].tag !== 2)
      $fatal(1, "labelled was %p", labelled);
    if (record.body[1] !== 20 || record.body[2] !== 3 ||
        record.labels[1].name != "inner")
      $fatal(1, "record was %p", record);
    if (records[1].head !== 8'hA5 || records[1].body[1] !== 20 ||
        records[1].body[2] !== 30 || records[1].labels[1].name != "inner")
      $fatal(1, "records[1] was %p", records[1]);
    if (records[0].labels[0].name != "first" || records[0].body[2] !== 0)
      $fatal(1, "records[0] was %p", records[0]);
    if (rows_in_queue[1][0] !== 44 || rows_in_queue[1][3] !== 1 ||
        rows_in_queue[0][0] !== 1)
      $fatal(1, "rows_in_queue was %p", rows_in_queue);
    if (wide[2] !== (100'd5 << 70) || wide[0] !== 100'bx)
      $fatal(1, "wide was %p", wide);
    if (wired[0] !== 8'h12 || wired[1] !== 8'h34)
      $fatal(1, "wired was %p", wired);
    if (bad_read !== 0)
      $fatal(1, "an index past the end read %0d, expected 0", bad_read);
    if (bad_constant_read !== 0)
      $fatal(1, "a constant index past the end read %0d, expected 0",
             bad_constant_read);
    if (bad_read_four_state !== 4'bx)
      $fatal(1, "an index past the end read %b, expected x",
             bad_read_four_state);
    if (bad_read_word != "")
      $fatal(1, "an index past the end read \"%s\", expected \"\"",
             bad_read_word);
    if (bad_row_read !== 0)
      $fatal(1, "a row past the end read %0d, expected 0", bad_row_read);
    if (sum_few !== 10)
      $fatal(1, "sum_of answered %0d, expected 10", sum_few);
    if (sum_many !== 600)
      $fatal(1, "sum_of_many answered %0d, expected 600", sum_many);
    if (from_result !== 4)
      $fatal(1, "a function's array result began with %0d, expected 4",
             from_result);
    if (local_kept !== 11)
      $fatal(1, "a task's local past the in-frame size held %0d, expected 11", local_kept);
    if (deep_equal_to_its_copy !== 1'b1 || deep_equal_after_a_write !== 1'b0)
      $fatal(1, "an array of large rows compared %b to its copy and %b after a write",
             deep_equal_to_its_copy, deep_equal_after_a_write);
    if (deep_rows_equal !== 1'b1 || deep_row[599] !== -1 || deep_row[0] !== 1000)
      $fatal(1, "a large row assigned from its neighbour ended with %0d",
             deep_row[599]);
    if (deep_bad_row_read !== 0)
      $fatal(1, "a large row past the end read %0d, expected 0",
             deep_bad_row_read);
    if (deep_by_value !== 1999 || deep_after_by_value !== -1)
      $fatal(1, "an array of large rows passed by value answered %0d and was left at %0d",
             deep_by_value, deep_after_by_value);
    if (deep_through_reference !== 1600 || deep_copy[1][599] !== 1599)
      $fatal(1, "a reference to an array of large rows read %0d, expected 1600",
             deep_through_reference);
    if (deep[0][0] !== 0 || deep[0][599] !== 609 || deep[1][0] !== 1000 ||
        deep[1][599] !== 1600)
      $fatal(1, "an array of large rows held %0d %0d %0d %0d", deep[0][0],
             deep[0][599], deep[1][0], deep[1][599]);
    $display("All checks passed");
  end
endmodule
