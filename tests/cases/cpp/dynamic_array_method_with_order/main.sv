module Top;
  int arr [];
  int rs [];
  int sorted_payload_0, sorted_payload_1, sorted_payload_2;
  int rsorted_payload_0, rsorted_payload_1, rsorted_payload_2;

  initial begin
    arr = new[3];
    arr[0] = 3;
    arr[1] = 1;
    arr[2] = 2;
    // LRM 7.12.2: sort with key = item; result ascending.
    arr.sort with (item);
    sorted_payload_0 = arr[0];
    sorted_payload_1 = arr[1];
    sorted_payload_2 = arr[2];

    rs = new[3];
    rs[0] = 10;
    rs[1] = 30;
    rs[2] = 20;
    // LRM 7.12.2: rsort with key = item; result descending.
    rs.rsort with (item);
    rsorted_payload_0 = rs[0];
    rsorted_payload_1 = rs[1];
    rsorted_payload_2 = rs[2];
  end
endmodule
