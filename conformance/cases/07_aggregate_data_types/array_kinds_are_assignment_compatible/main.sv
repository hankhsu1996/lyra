// A fixed-size unpacked array, a dynamic array, and a queue are assignment
// compatible with one another when their element types are equivalent, and the
// elements pair by the left-to-right order of each array. What the target
// declares and the source cannot supply stays the target's: a queue or dynamic
// array is resized to the source's element count, while a fixed-size array
// keeps the count it declares and so takes only a source of that many elements
// (LRM 7.6). Two arrays of one kind are assigned the same way where only the
// range of a faster-varying dimension is declared differently, and so are two
// associative arrays of one index type (LRM 7.9.9).
module Top;
  int rows_up [][0:3];
  int rows_down [][3:0];
  int queued_up [$][0:1];
  int queued_down [$][1:0];
  int keyed_up [string][0:1];
  int keyed_down [string][1:0];
  int fixed_up [2][0:1];
  int fixed_down [2][1:0];

  int fixed_source [3] = '{1, 2, 3};
  int dynamic_source [];
  int queue_source [$];

  int queue_from_fixed [$];
  int queue_from_dynamic [$];
  int fixed_from_dynamic [3];
  int fixed_from_queue [3];

  initial begin
    dynamic_source = new[3];
    dynamic_source[0] = 4;
    dynamic_source[1] = 5;
    dynamic_source[2] = 6;
    queue_source = '{7, 8, 9};

    queue_from_fixed = '{99};
    queue_from_dynamic = '{99};
    fixed_from_dynamic = '{99, 99, 99};
    fixed_from_queue = '{99, 99, 99};

    queue_from_fixed = fixed_source;
    queue_from_dynamic = dynamic_source;
    fixed_from_dynamic = dynamic_source;
    fixed_from_queue = queue_source;

    rows_up = new[1];
    rows_up[0] = '{1, 2, 3, 4};
    rows_down = rows_up;
    queued_up.push_back('{5, 6});
    queued_down = queued_up;
    keyed_up["k"] = '{7, 8};
    keyed_down = keyed_up;
    fixed_up = '{'{1, 2}, '{3, 4}};
    fixed_down = fixed_up;
  end

  final begin
    if (rows_down.size() !== 1 || rows_down[0][3] !== 1 ||
        rows_down[0][0] !== 4)
      $fatal(1, "a dynamic array of rows declared the other way was %p",
             rows_down);
    if (queued_down.size() !== 1 || queued_down[0][1] !== 5 ||
        queued_down[0][0] !== 6)
      $fatal(1, "a queue of rows declared the other way was %p", queued_down);
    if (keyed_down["k"][1] !== 7 || keyed_down["k"][0] !== 8)
      $fatal(1, "an associative array of rows declared the other way was %p",
             keyed_down);
    if (fixed_down[0][1] !== 1 || fixed_down[1][0] !== 4)
      $fatal(1, "a fixed-size array of rows declared the other way was %p",
             fixed_down);

    if (queue_from_fixed.size() !== 3)
      $fatal(1, "a queue given a fixed-size array of 3 held %0d elements",
             queue_from_fixed.size());
    if (queue_from_fixed[0] !== 1 || queue_from_fixed[2] !== 3)
      $fatal(1, "a queue given a fixed-size array read %0d then %0d",
             queue_from_fixed[0], queue_from_fixed[2]);

    if (queue_from_dynamic.size() !== 3)
      $fatal(1, "a queue given a dynamic array of 3 held %0d elements",
             queue_from_dynamic.size());
    if (queue_from_dynamic[0] !== 4 || queue_from_dynamic[2] !== 6)
      $fatal(1, "a queue given a dynamic array read %0d then %0d",
             queue_from_dynamic[0], queue_from_dynamic[2]);

    if (fixed_from_dynamic[0] !== 4 || fixed_from_dynamic[2] !== 6)
      $fatal(1, "a fixed-size array given a dynamic array read %0d then %0d",
             fixed_from_dynamic[0], fixed_from_dynamic[2]);

    if (fixed_from_queue[0] !== 7 || fixed_from_queue[2] !== 9)
      $fatal(1, "a fixed-size array given a queue read %0d then %0d",
             fixed_from_queue[0], fixed_from_queue[2]);

    $display("All checks passed");
  end
endmodule
