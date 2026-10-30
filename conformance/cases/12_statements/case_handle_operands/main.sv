// A case statement compares its expression against each item's expressions with
// exact equality and takes the first item that matches, or the default item
// when none does (LRM 12.5). Over object handles that comparison is the one
// LRM 8.4 defines -- which object is named, with null admitted as an item
// expression like any other -- so a case selects by object identity, and an
// item naming a handle that was reassigned since is not the item taken.
module Top;
  class Box;
    int held;
  endclass

  Box first;
  Box second;
  Box empty;
  Box chosen;

  int selected_object;
  int selected_from_a_list;
  int selected_null;
  int defaulted;

  initial begin
    selected_object = -1;
    selected_from_a_list = -1;
    selected_null = -1;
    defaulted = -1;

    first = new;
    second = new;

    chosen = second;
    case (chosen)
      null:   selected_object = 0;
      first:  selected_object = 1;
      second: selected_object = 2;
      default: selected_object = 99;
    endcase

    chosen = first;
    case (chosen)
      null:            selected_from_a_list = 0;
      second, first:   selected_from_a_list = 23;
      default:         selected_from_a_list = 99;
    endcase

    chosen = empty;
    case (chosen)
      first:   selected_null = 1;
      second:  selected_null = 2;
      null:    selected_null = 7;
      default: selected_null = 99;
    endcase

    chosen = new;
    case (chosen)
      null:    defaulted = 0;
      first:   defaulted = 1;
      second:  defaulted = 2;
      default: defaulted = 100;
    endcase
  end

  final begin
    if (selected_object !== 2)
      $fatal(1, "selected_object was %0d, expected 2", selected_object);
    if (selected_from_a_list !== 23)
      $fatal(1, "selected_from_a_list was %0d, expected 23",
             selected_from_a_list);
    if (selected_null !== 7)
      $fatal(1, "selected_null was %0d, expected 7", selected_null);
    if (defaulted !== 100)
      $fatal(1, "defaulted was %0d, expected 100", defaulted);
    $display("All checks passed");
  end
endmodule
