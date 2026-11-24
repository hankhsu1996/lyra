// A tagged union may hold a member of a dynamic type -- a string, a class
// handle, a queue, a structure holding one, another tagged union holding one
// (LRM 7.3, 7.3.2) -- and its tag is what says that member is there. A copy
// carries the member and is independent of the original afterwards; assigning
// a value tagged with another member replaces it, and one tagged with the same
// member again holds the new value. Each holds its value however it is handed
// on: a local assigned or initialized by a tagged expression, returned,
// assigned from a call, a local initialized from one or copied from another,
// passed by value, pushed into a queue, written into a module variable, a
// class property, a structure member or an array element, and copied around
// and replaced many times. The texts are short and long alike, since a text's
// length may decide how it is held.
module Top;
  class Box;
    string label;
    function new(string label);
      this.label = label;
    endfunction
  endclass

  typedef struct {
    string name;
    int count;
  } named_t;

  typedef union tagged {
    int Count;
    string Text;
  } inner_t;

  typedef union tagged {
    int Count;
    string Text;
    Box Handle;
    string Lines[$];
    named_t Named;
    inner_t Inner;
  } item_t;

  typedef struct {
    int ahead;
    item_t item;
  } wrapped_t;

  class Holder;
    item_t item;
  endclass

  // The members that carry a text: Text, Handle, Lines, Named and Inner.
  localparam int kKinds = 5;
  // The places a value is handed to below.
  localparam int kPositions = 10;
  localparam string kLong =
      "a long text, of many more characters than the short one";

  // A value of the member `kind` names, carrying `text`, built by a tagged
  // expression into a local and returned.
  function automatic item_t Made(int kind, string text);
    item_t made;
    Box box;
    string lines[$];
    made = tagged Count -1;
    case (kind)
      0: made = tagged Text text;
      1: begin
        box = new(text);
        made = tagged Handle box;
      end
      2: begin
        lines.push_back(text);
        made = tagged Lines lines;
      end
      3: made = tagged Named '{name: text, count: 1};
      4: made = tagged Inner (tagged Text text);
      default: made = tagged Count kind;
    endcase
    return made;
  endfunction

  // The text `item` carries, whichever member holds it.
  function automatic string Said(item_t item);
    if (item matches tagged Text .*) return item.Text;
    if (item matches tagged Handle .*) return item.Handle.label;
    if (item matches tagged Lines .*) return item.Lines[0];
    if (item matches tagged Named .*) return item.Named.name;
    if (item matches tagged Inner .*) begin
      if (item.Inner matches tagged Text .*) return item.Inner.Text;
    end
    return "no text";
  endfunction

  function automatic string InitializedByATaggedExpression(
      int kind, string text);
    Box box = new(text);
    string lines[$] = '{text};
    item_t as_text = tagged Text text;
    item_t as_handle = tagged Handle box;
    item_t as_lines = tagged Lines lines;
    item_t as_named = tagged Named '{name: text, count: 1};
    item_t as_inner = tagged Inner (tagged Text text);
    case (kind)
      0: return Said(as_text);
      1: return Said(as_handle);
      2: return Said(as_lines);
      3: return Said(as_named);
      4: return Said(as_inner);
      default: return "no kind";
    endcase
  endfunction

  function automatic string AssignedFromACall(int kind, string text);
    item_t local_item;
    local_item = tagged Count 0;
    local_item = Made(kind, text);
    return Said(local_item);
  endfunction

  function automatic string InitializedFromACall(int kind, string text);
    item_t local_item = Made(kind, text);
    return Said(local_item);
  endfunction

  function automatic string CopiedFromALocal(int kind, string text);
    item_t first = Made(kind, text);
    item_t second;
    second = tagged Count 0;
    second = first;
    first = tagged Count 1;
    return Said(second);
  endfunction

  // A value passed on, replaced and passed on again, as many times as asked.
  function automatic item_t Retold(item_t given, int rounds);
    item_t held = given;
    for (int i = 0; i < rounds; i++) begin
      item_t next = held;
      held = tagged Count i;
      held = next;
    end
    return held;
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

  item_t module_item;
  item_t pushed[$];
  item_t by_kind[kKinds];
  wrapped_t wrapped;
  Holder holder;
  int held_checks;

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

    held_checks = 0;
    for (int kind = 0; kind < kKinds; kind++) begin
      for (int length = 0; length < 2; length++) begin
        string text;
        string said[kPositions];
        text = $sformatf("%s, kind %0d", length == 0 ? "short" : kLong, kind);

        said[0] = Said(Made(kind, text));
        said[1] = AssignedFromACall(kind, text);
        said[2] = InitializedFromACall(kind, text);
        said[3] = CopiedFromALocal(kind, text);

        module_item = tagged Count 0;
        module_item = Made(kind, text);
        said[4] = Said(module_item);

        pushed.push_back(Made(kind, text));
        said[5] = Said(pushed[pushed.size() - 1]);

        holder = new;
        holder.item = Made(kind, text);
        said[6] = Said(holder.item);
        holder = null;

        wrapped.item = Made(kind, text);
        by_kind[kind] = wrapped.item;
        wrapped.item = tagged Count 0;
        said[7] = Said(by_kind[kind]);

        said[8] = Said(Retold(by_kind[kind], 100));
        said[9] = InitializedByATaggedExpression(kind, text);

        for (int position = 0; position < kPositions; position++) begin
          if (said[position] != text)
            $fatal(1, "kind %0d at position %0d read '%s', expected '%s'",
                   kind, position, said[position], text);
          held_checks++;
        end
      end
    end
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
    if (held_checks !== kKinds * 2 * kPositions)
      $fatal(1, "%0d of the handed-on values were checked, expected %0d",
             held_checks, kKinds * 2 * kPositions);
    $display("All checks passed");
  end
endmodule
