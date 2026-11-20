// A write through an invalid index of a queue or a dynamic array is ignored
// (LRM 7.10.1, 7.4.5), and so is any write that reaches further into the
// element such an index names: a method that grows it, a write to one of its
// own elements, or one to a member of a structure it holds. The array keeps
// every element it held, with the value it held, whatever the elements own --
// strings, queues of strings, or structures holding them -- and an element a
// later write appends at $+1 starts empty. That holds of an array a module
// declares and of one an automatic subroutine declares alike.
module Top;
  typedef string strings_t [$];
  typedef struct {
    int tag;
    string name;
    strings_t notes;
  } record_t;

  strings_t nested [$];
  string grid [][];
  record_t records [$];
  int negative_index = -1;

  // The same writes to arrays an automatic subroutine declares, which no
  // process waits on.
  function automatic bit locals_keep_their_elements(int invalid);
    strings_t queue_of_queues [$];
    string array_of_arrays [][];
    queue_of_queues.push_back('{"a", "b"});
    queue_of_queues[5].push_back("lost");
    queue_of_queues[5][0] = "lost again";
    queue_of_queues[invalid][3] = "never";
    array_of_arrays = new[1];
    array_of_arrays[0] = new[2];
    array_of_arrays[0][1] = "kept";
    array_of_arrays[4] = new[3];
    array_of_arrays[4][0] = "gone";
    array_of_arrays[invalid][1] = "never";
    return queue_of_queues.size() == 1 && queue_of_queues[0].size() == 2 &&
           queue_of_queues[0][0] == "a" && queue_of_queues[0][1] == "b" &&
           array_of_arrays.size() == 1 && array_of_arrays[0].size() == 2 &&
           array_of_arrays[0][0] == "" && array_of_arrays[0][1] == "kept";
  endfunction

  initial begin
    if (!locals_keep_their_elements(negative_index))
      $fatal(1, "an automatic array changed under ignored writes");
    nested.push_back('{"a", "b"});
    nested[5].push_back("lost");
    nested[5][0] = "lost again";
    nested[negative_index][3] = "never";
    nested[$+2].push_back("past the end");
    if (nested.size() !== 1 || nested[0].size() !== 2 || nested[0][0] != "a" ||
        nested[0][1] != "b")
      $fatal(1, "a queue of queues read as %p after ignored writes", nested);
    nested[$+1].push_back("appended");
    if (nested.size() !== 2 || nested[1].size() !== 1 ||
        nested[1][0] != "appended")
      $fatal(1, "an element appended after ignored writes read as %p",
             nested[1]);

    grid = new[1];
    grid[0] = new[2];
    grid[0][1] = "kept";
    grid[4] = new[3];
    grid[4][0] = "gone";
    grid[negative_index][1] = "never";
    if (grid.size() !== 1 || grid[0].size() !== 2 || grid[0][0] != "" ||
        grid[0][1] != "kept")
      $fatal(1, "a dynamic array of arrays read as %p after ignored writes",
             grid);

    records.push_back('{tag: 1, name: "first", notes: '{"one"}});
    records[3].name = "nowhere";
    records[3].notes.push_back("nowhere");
    records[negative_index].notes[0] = "never";
    if (records.size() !== 1 || records[0].tag !== 1 ||
        records[0].name != "first" || records[0].notes.size() !== 1 ||
        records[0].notes[0] != "one")
      $fatal(1, "a queue of records read as %p after ignored writes", records);
    $display("All checks passed");
  end
endmodule
