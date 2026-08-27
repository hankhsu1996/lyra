// A string variable holds its characters rather than a reference to them, so
// assigning one string to another copies them and the two vary independently
// from then on. A string is an ordinary variable on those terms wherever one
// can be declared, including as the element type of a fixed-size, dynamic, or
// queue array, and an element never assigned holds the empty string
// (LRM 6.16, 7.4).
module Top;
  string original;
  string copied;
  string original_after_copy_written;
  string copied_after_original_written;

  string names_fixed [3];
  string names_dynamic [];
  string names_queue [$];

  string never_assigned_element = "unset";
  int never_assigned_element_len = -1;

  initial begin
    original = "alpha";
    copied = original;
    copied.putc(0, "A");
    original_after_copy_written = original;
    original = "omega";
    copied_after_original_written = copied;

    names_fixed[0] = "first";
    names_fixed[2] = "third";
    never_assigned_element = names_fixed[1];
    never_assigned_element_len = names_fixed[1].len();

    names_dynamic = new [2];
    names_dynamic[0] = "left";
    names_dynamic[1] = "right";

    names_queue.push_back("front");
    names_queue.push_back("back");
  end

  final begin
    if (original_after_copy_written != "alpha")
      $fatal(1, "original_after_copy_written was \"%s\", expected \"alpha\"",
             original_after_copy_written);
    if (copied_after_original_written != "Alpha")
      $fatal(1, "copied_after_original_written was \"%s\", expected \"Alpha\"",
             copied_after_original_written);
    if (original != "omega")
      $fatal(1, "original was \"%s\", expected \"omega\"", original);
    if (copied != "Alpha")
      $fatal(1, "copied was \"%s\", expected \"Alpha\"", copied);

    if (names_fixed[0] != "first")
      $fatal(1, "names_fixed[0] was \"%s\", expected \"first\"",
             names_fixed[0]);
    if (names_fixed[2] != "third")
      $fatal(1, "names_fixed[2] was \"%s\", expected \"third\"",
             names_fixed[2]);
    if (never_assigned_element != "")
      $fatal(1, "never_assigned_element was \"%s\", expected \"\"",
             never_assigned_element);
    if (never_assigned_element_len !== 0)
      $fatal(1, "never_assigned_element_len was %0d, expected 0",
             never_assigned_element_len);

    if (names_dynamic[1] != "right")
      $fatal(1, "names_dynamic[1] was \"%s\", expected \"right\"",
             names_dynamic[1]);
    if (names_queue.size() !== 2)
      $fatal(1, "names_queue.size() was %0d, expected 2", names_queue.size());
    if (names_queue[0] != "front")
      $fatal(1, "names_queue[0] was \"%s\", expected \"front\"",
             names_queue[0]);
    $display("All checks passed");
  end
endmodule
