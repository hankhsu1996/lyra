module Top;
  function automatic logic [7:0] make_packed();
    return 8'hAB;
  endfunction

  logic [7:0] a;
  logic [7:0] b;
  int arr[4];

  logic [3:0] concat_rsel_rt;
  logic concat_bsel_rt;
  logic [3:0] repl_rsel_rt;
  logic [3:0] concat_part_rt;
  int computed_idx_rt;
  logic fn_index_rt;

  initial begin
    a = 8'h3C;
    b = 8'h9A;

    concat_rsel_rt = {a, b}[11:8];

    concat_bsel_rt = {a, b}[11];

    repl_rsel_rt = {4{4'hA}}[15:12];

    concat_part_rt = {a, b}[4+:4];

    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    computed_idx_rt = arr[1+1];

    fn_index_rt = make_packed()[3];
  end
endmodule
