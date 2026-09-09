// A fixed-size unpacked array, a dynamic array, and a queue are assignment
// compatible with one another when their element types are equivalent, and the
// elements pair by the left-to-right order of each array. What the target
// declares and the source cannot supply stays the target's: a queue or dynamic
// array is resized to the source's element count, while a fixed-size array
// keeps the count it declares and so takes only a source of that many elements
// (LRM 7.6).
module Top;
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
  end

  final begin
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
