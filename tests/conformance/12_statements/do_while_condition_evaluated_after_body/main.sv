// A do...while loop evaluates its control expression after the statement, so
// each pass runs the body first and the loop continues only while that
// expression holds (LRM 12.7.5). A loop body is one statement, so a begin-end
// block and a bare assignment are both bodies (LRM 12.7).
module Top;
  int counter;
  int sum;
  int stepped;
  int countdown;
  int total;

  initial begin
    counter = 0;
    sum = 0;
    do begin
      sum = sum + counter;
      counter = counter + 1;
    end while (counter < 5);

    stepped = 0;
    do stepped = stepped + 1; while (stepped < 4);

    countdown = 3;
    total = 0;
    do begin
      total = total + countdown;
      countdown = countdown - 1;
    end while (countdown > 0);
  end

  final begin
    if (counter !== 5) $fatal(1, "counter was %0d, expected 5", counter);
    if (sum !== 10) $fatal(1, "sum was %0d, expected 10", sum);
    if (stepped !== 4) $fatal(1, "stepped was %0d, expected 4", stepped);
    if (countdown !== 0) $fatal(1, "countdown was %0d, expected 0", countdown);
    if (total !== 6) $fatal(1, "total was %0d, expected 6", total);
    $display("All checks passed");
  end
endmodule
