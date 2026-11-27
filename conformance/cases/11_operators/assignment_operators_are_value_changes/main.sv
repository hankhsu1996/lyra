// An assignment operator is a blocking assignment to its target (LRM 11.4.1),
// so the change it makes is an update event other processes are sensitive to
// (LRM 4.3): an event control on the target resumes, and an `always_comb`
// reading it evaluates again (LRM 9.2.2.2). That holds whatever the target is
// -- a whole variable, an element of an array, an entry of an associative
// array the operator makes or one already there, or some bits of a vector --
// and for an increment as for any other assignment operator.
module Top;
  int whole;
  int elements[4];
  int entries[int] = '{7: 1};
  logic [255:0] wide = '0;

  int whole_seen = -1;
  int element_seen = -1;
  int made_entry_seen = -1;
  int entry_seen = -1;
  int bits_seen = -1;
  int sum;

  always_comb sum = whole + elements[2] + entries[7];

  initial begin
    @(whole);
    whole_seen = whole;
  end
  initial begin
    @(elements[2]);
    element_seen = elements[2];
  end
  initial begin
    @(entries[5]);
    made_entry_seen = entries[5];
  end
  initial begin
    @(entries[7]);
    entry_seen = entries[7];
  end
  initial begin
    @(wide[3:0]);
    bits_seen = int'(wide[3:0]);
  end

  initial begin
    #1;
    whole += 3;
    elements[2] += 4;
    entries[5]++;
    entries[7] <<= 2;
    wide[3:0] += 6;
  end

  final begin
    if (whole_seen != 3)
      $fatal(1, "a wait on a whole variable saw %0d, expected 3", whole_seen);
    if (element_seen != 4)
      $fatal(1, "a wait on an element saw %0d, expected 4", element_seen);
    if (made_entry_seen != 1)
      $fatal(1, "a wait on an entry the operator made saw %0d, expected 1",
             made_entry_seen);
    if (entry_seen != 4)
      $fatal(1, "a wait on an entry already there saw %0d, expected 4",
             entry_seen);
    if (bits_seen != 6)
      $fatal(1, "a wait on some bits saw %0d, expected 6", bits_seen);
    if (sum != 11)
      $fatal(1, "an always_comb reading the targets gave %0d, expected 11", sum);
    $display("All checks passed");
  end
endmodule
