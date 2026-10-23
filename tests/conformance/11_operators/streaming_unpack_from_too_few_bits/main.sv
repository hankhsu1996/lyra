// @error: holds fewer bits than the variables it is unpacked into need
//
// Unpacking a stream that holds fewer bits than its targets need is an error,
// and where the stream's width is the running program's, a queue holding one
// byte here, the error is raised when the assignment runs (LRM 11.4.14.3).
module Top;
  int word;

  initial begin
    byte bytes[$];
    bytes = {8'h01};
    {>>{word}} = bytes;
  end
endmodule
