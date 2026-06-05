module Top;
  // Signed-integer order anchors LRM 7.12.2: relational < uses the element
  // type's natural ordering, so negatives sort below positives.
  int sorted [];
  int rsorted [];
  int empty_sorted [];
  int single_sorted [];
  int sorted_dup [];
  initial begin
    sorted = new[4];
    sorted[0] = 3;
    sorted[1] = -1;
    sorted[2] = 2;
    sorted[3] = -5;
    sorted.sort();

    rsorted = new[4];
    rsorted[0] = 3;
    rsorted[1] = -1;
    rsorted[2] = 2;
    rsorted[3] = -5;
    rsorted.rsort();

    // Empty and single-element are no-ops for both sort and rsort.
    empty_sorted.sort();

    single_sorted = new[1];
    single_sorted[0] = 9;
    single_sorted.rsort();

    // Duplicates preserve count.
    sorted_dup = new[5];
    sorted_dup[0] = 2;
    sorted_dup[1] = 1;
    sorted_dup[2] = 3;
    sorted_dup[3] = 1;
    sorted_dup[4] = 2;
    sorted_dup.sort();
  end
endmodule
