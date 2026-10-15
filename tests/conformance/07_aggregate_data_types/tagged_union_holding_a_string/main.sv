// A tagged union may hold a member of a dynamic type, a string among them (LRM
// 7.3, 7.3.2), and its tag is what says that member is there. A copy carries
// the string and is independent of the original afterwards; assigning a value
// tagged with another member replaces the string, and one tagged with the
// string member again holds the new string; and each holds its value however
// many times it is copied around and replaced, in a variable, an array
// element, a queue element and a subroutine's local.
module Top;
  typedef union tagged {
    int Count;
    string Text;
  } item_t;

  function automatic item_t Retold(item_t given, string suffix);
    item_t local_copy = given;
    if (local_copy matches tagged Text .t) local_copy = tagged Text {t, suffix};
    return local_copy;
  endfunction

  item_t original;
  item_t copy;
  item_t row[2];
  item_t queue_of[$];

  string copy_text;
  string copy_text_after_original_retagged;
  int original_count;
  string original_text_again;
  string row_text;
  string queue_text;
  string retold_text;
  string rounds_text;

  initial begin
    original = tagged Text "first";
    copy = original;
    copy_text = copy.Text;

    original = tagged Count 3;
    copy_text_after_original_retagged = copy.Text;
    original_count = original.Count;

    original = tagged Text "second";
    original_text_again = original.Text;

    row[0] = original;
    row[1] = row[0];
    row[0] = tagged Count 1;
    row_text = row[1].Text;

    queue_of.push_back(copy);
    queue_of.push_back(tagged Count 2);
    queue_of.delete(1);
    queue_text = queue_of[0].Text;

    retold_text = Retold(copy, "+").Text;

    for (int i = 0; i < 100; i++) begin
      copy = Retold(copy, "");
      copy = tagged Count i;
      copy = tagged Text $sformatf("round %0d", i);
    end
    rounds_text = copy.Text;
  end

  final begin
    if (copy_text != "first")
      $fatal(1, "a copy read %s, expected first", copy_text);
    if (copy_text_after_original_retagged != "first")
      $fatal(1, "a copy read %s once its original was retagged, expected first",
             copy_text_after_original_retagged);
    if (original_count !== 3)
      $fatal(1, "the retagged original read %0d, expected 3", original_count);
    if (original_text_again != "second")
      $fatal(1, "the original read %s, expected second", original_text_again);
    if (row_text != "second")
      $fatal(1, "an array element read %s, expected second", row_text);
    if (queue_text != "first")
      $fatal(1, "a queue element read %s, expected first", queue_text);
    if (retold_text != "first+")
      $fatal(1, "a subroutine's local answered %s, expected first+",
             retold_text);
    if (rounds_text != "round 99")
      $fatal(1, "after the rounds the copy read %s, expected round 99",
             rounds_text);
    $display("All checks passed");
  end
endmodule
