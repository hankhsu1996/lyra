// LRM 7.4.5 sends a read at an invalid index to Table 7-1, and that table gives
// an unpacked struct each member's own table value except where the member's
// declaration assigns one (LRM 7.2.2), which is then that member's value. What a
// container was filled with does not enter that answer, so a container written
// as an assignment pattern answers an invalid read with the member's declared
// value and not with whatever the pattern stored.
module Top;
  typedef struct {
    int declared = 7;
    int plain;
  } Element;

  Element written;
  Element element_queue[$];
  Element element_dynamic[];
  Element by_index[int];

  int queue_declared = -1;
  int dynamic_declared = -1;
  int index_declared = -1;

  initial begin
    written.declared = 3;
    element_queue = '{written};
    element_dynamic = '{written};
    by_index = '{0: written};

    queue_declared = element_queue[9].declared;
    dynamic_declared = element_dynamic[9].declared;
    index_declared = by_index[5].declared;
  end

  final begin
    if (queue_declared !== 7)
      $fatal(1, "an invalid read of a written queue gave %0d, expected 7", queue_declared);
    if (dynamic_declared !== 7)
      $fatal(1, "an invalid read of a written dynamic array gave %0d, expected 7",
             dynamic_declared);
    if (index_declared !== 7)
      $fatal(1, "a missing entry of a written associative array gave %0d, expected 7",
             index_declared);
    $display("All checks passed");
  end
endmodule
