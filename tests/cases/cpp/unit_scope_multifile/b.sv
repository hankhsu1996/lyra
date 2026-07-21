// A second, independent compilation-unit input. Its `$unit`-scope `m` shares a
// name with the other file's but is a distinct cell in a distinct anonymous
// unit; it is not visible to the other file and is never read here.
int m = 100;
