// $fclose releases a channel and $fopen shall reuse channels that have been
// closed, so opening a second file after closing the first hands back the same
// descriptor (LRM 21.3.1).
module Top;
  int first;
  int second;
  int third;
  int held;
  int also_held;

  initial begin
    first = $fopen("first.txt");
    $fclose(first);
    second = $fopen("second.txt");
    $fclose(second);
    third = $fopen("third.txt");
    $fclose(third);

    // Two channels open at the same time, which a released one and a fresh one
    // are not, so the equality checked above reads as reuse rather than as one
    // answer given to every call.
    held = $fopen("held.txt");
    also_held = $fopen("also_held.txt");
    $fclose(held);
    $fclose(also_held);
  end

  final begin
    if (first === 0)
      $fatal(1, "the first channel could not be opened");
    if (also_held === held)
      $fatal(1, "a channel taken while %h was still held was %h too", held,
             also_held);
    if (second !== first)
      $fatal(1, "the channel after a close was %h, expected the released %h",
             second, first);
    if (third !== first)
      $fatal(1, "the channel after a second close was %h, expected %h",
             third, first);
    $display("All checks passed");
  end
endmodule
