// first() and last() assign the smallest and the largest index an associative
// array holds to their argument and return 1, or return 0 when the array has no
// entries; next() and prev() move the argument to the neighbouring index and
// return 1, or return 0 and leave the argument alone once there is no such
// neighbour. Smallest and largest follow the order the index type imposes:
// lexicographic for a string index and signed numerical for a signed integral
// one (LRM 7.9.4, 7.9.5, 7.9.6, 7.9.7, 7.8.2, 7.8.4).
module Top;
  int by_text [string];
  string text_key;
  string forward_keys [3];
  string backward_keys [3];
  int forward_count;
  int backward_count;
  int first_status;
  int last_status;
  string last_key;
  int next_past_end;
  string key_after_end;
  int prev_past_begin;
  string key_after_begin;

  int no_entries [string];
  string unused_key;
  int empty_status;

  int by_number [int];
  int number_key;
  int forward_numbers [3];
  int number_count;

  int signed_keys [int];
  int signed_first;
  int signed_last;


  initial begin
    by_text["banana"] = 2;
    by_text["apple"] = 1;
    by_text["cherry"] = 3;

    forward_count = 0;
    first_status = by_text.first(text_key);
    if (first_status) begin
      do begin
        forward_keys[forward_count] = text_key;
        forward_count = forward_count + 1;
      end while (by_text.next(text_key));
    end
    next_past_end = by_text.next(text_key);
    key_after_end = text_key;

    backward_count = 0;
    last_status = by_text.last(text_key);
    last_key = text_key;
    if (last_status) begin
      do begin
        backward_keys[backward_count] = text_key;
        backward_count = backward_count + 1;
      end while (by_text.prev(text_key));
    end
    prev_past_begin = by_text.prev(text_key);
    key_after_begin = text_key;

    empty_status = no_entries.first(unused_key);

    by_number[10] = 1;
    by_number[30] = 3;
    by_number[20] = 2;
    number_count = 0;
    if (by_number.first(number_key)) begin
      do begin
        forward_numbers[number_count] = number_key;
        number_count = number_count + 1;
      end while (by_number.next(number_key));
    end

    signed_keys[5] = 1;
    signed_keys[-5] = 2;
    void'(signed_keys.first(signed_first));
    void'(signed_keys.last(signed_last));

  end

  final begin
    if (first_status !== 1)
      $fatal(1, "first_status was %0d, expected 1", first_status);
    if (last_status !== 1)
      $fatal(1, "last_status was %0d, expected 1", last_status);
    if (last_key !== "cherry")
      $fatal(1, "last_key was \"%s\", expected \"cherry\"", last_key);
    if (forward_count !== 3)
      $fatal(1, "forward_count was %0d, expected 3", forward_count);
    if (forward_keys[0] !== "apple")
      $fatal(1, "forward_keys[0] was \"%s\", expected \"apple\"",
             forward_keys[0]);
    if (forward_keys[1] !== "banana")
      $fatal(1, "forward_keys[1] was \"%s\", expected \"banana\"",
             forward_keys[1]);
    if (forward_keys[2] !== "cherry")
      $fatal(1, "forward_keys[2] was \"%s\", expected \"cherry\"",
             forward_keys[2]);
    if (next_past_end !== 0)
      $fatal(1, "next_past_end was %0d, expected 0", next_past_end);
    if (key_after_end !== "cherry")
      $fatal(1, "key_after_end was \"%s\", expected \"cherry\"", key_after_end);

    if (backward_count !== 3)
      $fatal(1, "backward_count was %0d, expected 3", backward_count);
    if (backward_keys[0] !== "cherry")
      $fatal(1, "backward_keys[0] was \"%s\", expected \"cherry\"",
             backward_keys[0]);
    if (backward_keys[1] !== "banana")
      $fatal(1, "backward_keys[1] was \"%s\", expected \"banana\"",
             backward_keys[1]);
    if (backward_keys[2] !== "apple")
      $fatal(1, "backward_keys[2] was \"%s\", expected \"apple\"",
             backward_keys[2]);
    if (prev_past_begin !== 0)
      $fatal(1, "prev_past_begin was %0d, expected 0", prev_past_begin);
    if (key_after_begin !== "apple")
      $fatal(1, "key_after_begin was \"%s\", expected \"apple\"",
             key_after_begin);

    if (empty_status !== 0)
      $fatal(1, "empty_status was %0d, expected 0", empty_status);

    if (number_count !== 3)
      $fatal(1, "number_count was %0d, expected 3", number_count);
    if (forward_numbers[0] !== 10)
      $fatal(1, "forward_numbers[0] was %0d, expected 10", forward_numbers[0]);
    if (forward_numbers[1] !== 20)
      $fatal(1, "forward_numbers[1] was %0d, expected 20", forward_numbers[1]);
    if (forward_numbers[2] !== 30)
      $fatal(1, "forward_numbers[2] was %0d, expected 30", forward_numbers[2]);

    if (signed_first !== -5)
      $fatal(1, "signed_first was %0d, expected -5", signed_first);
    if (signed_last !== 5)
      $fatal(1, "signed_last was %0d, expected 5", signed_last);

    $display("All checks passed");
  end
endmodule
