// The target of a nonblocking assignment may be a whole fixed-size unpacked
// array, a slice of one, a single element, or a member of a structure. The
// statement does not change the target where it stands: it evaluates the
// right-hand side and schedules the update, so a read taken afterwards in the
// same time step still sees the value the target had, and only once the
// update has been applied does the new value appear. An assignment whose
// left-hand side is a slice updates that whole slice at once, leaving the
// elements outside it as they were (LRM 10.4.2, 7.6).
module Top;
  typedef struct {
    int count;
    int spare;
  } entry_t;

  int whole [3] = '{1, 2, 3};
  int sliced [4] = '{1, 2, 3, 4};
  int element [3] = '{1, 2, 3};
  entry_t record;

  int source [3] = '{70, 80, 90};
  int slice_source [2] = '{77, 88};

  int whole_before;
  int sliced_before;
  int element_before;
  int record_before;

  initial begin
    record.count = 5;
    record.spare = 6;

    whole <= source;
    sliced[1 +: 2] <= slice_source;
    element[2] <= 99;
    record.count <= 55;

    whole_before = whole[0];
    sliced_before = sliced[1];
    element_before = element[2];
    record_before = record.count;

    #1;
  end

  final begin
    if (whole_before !== 1)
      $fatal(1, "whole_before was %0d, expected 1", whole_before);
    if (sliced_before !== 2)
      $fatal(1, "sliced_before was %0d, expected 2", sliced_before);
    if (element_before !== 3)
      $fatal(1, "element_before was %0d, expected 3", element_before);
    if (record_before !== 5)
      $fatal(1, "record_before was %0d, expected 5", record_before);

    if (whole[0] !== 70) $fatal(1, "whole[0] was %0d, expected 70", whole[0]);
    if (whole[2] !== 90) $fatal(1, "whole[2] was %0d, expected 90", whole[2]);

    if (sliced[0] !== 1)
      $fatal(1, "sliced[0] was %0d, expected 1", sliced[0]);
    if (sliced[1] !== 77)
      $fatal(1, "sliced[1] was %0d, expected 77", sliced[1]);
    if (sliced[2] !== 88)
      $fatal(1, "sliced[2] was %0d, expected 88", sliced[2]);
    if (sliced[3] !== 4)
      $fatal(1, "sliced[3] was %0d, expected 4", sliced[3]);

    if (element[1] !== 2)
      $fatal(1, "element[1] was %0d, expected 2", element[1]);
    if (element[2] !== 99)
      $fatal(1, "element[2] was %0d, expected 99", element[2]);

    if (record.count !== 55)
      $fatal(1, "record.count was %0d, expected 55", record.count);
    if (record.spare !== 6)
      $fatal(1, "record.spare was %0d, expected 6", record.spare);
    $display("All checks passed");
  end
endmodule
