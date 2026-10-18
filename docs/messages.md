# Messages

The rules for any text Lyra prints for a user to read: a diagnostic at compile time, a report while
a design runs, a refusal of the command line. A message is one more line of the stream the front end
writes, in the same form, so a reader takes in every line the same way without knowing which part of
the tool wrote it. The front end's form is clang's, and rustc, Swift and Go write theirs the same
way.

## The line

A line opens with where it is about, when it is about a place, and then one of three words:

```
t.sv:4:5: error: 'randcase' is not yet supported
t.sv:3:3: error: the size given to a dynamic array's new[] is negative
note: in 'Test' at time 0
```

- `error:`, `warning:` or `note:`. A failure of the compiler's own opens with `internal error:`
  after the first word. Nothing else is printed before the message: no tool name, no bracketed
  label, no `fatal:` for a report of Lyra's own.
- A report made while a design runs is placed where the running procedure is written. Which instance
  and what time are context, so they are a note of their own.
- The line a design's own severity task prints (`$info`, `$warning`, `$error`, `$fatal`, and an
  assertion's default report) uses the severity that task names (LRM 20.10), since the line is the
  design's.

## The text

- **Lowercase first letter and no final period.** The text continues the `file:line:col: error:`
  that the printer puts in front of it, so it is not the start of a sentence. A first word that is
  an acronym keeps its capitals (`DPI-C`, `PATH`).
- **One sentence.** A second part follows a semicolon. No colon inside the text: the line already
  holds two.
- **It says what is wrong, and only that.** What the user can do about it is a note, written as an
  instruction: `note: name one with '--cxx'`.
- **The user's own names in single quotes**: an identifier, a keyword, an option, a file path. Text
  a system call or another program returned goes in parentheses.
- **The user's words.** Words of the language, as the standard uses them, and words of Lyra's
  command line. Never a word for how Lyra works inside -- lowering, a closure, the front end.
- **No clause number of the standard.** The rule is said in words; the number belongs in the comment
  beside the code that implements it.
- **Something Lyra does not carry out yet says so in one form**: `'randcase' is not yet supported`.
  It names the construct, and never tells the reader that correct code is wrong.

## What the build checks

A message's text is written where it is reported, as a literal format, and that literal is checked
when Lyra is built: a first letter that is not lowercase (unless the first word is an acronym), a
final period, a colon followed by a space, a second sentence, a line break, a backtick or a double
quote, a contraction, a clause number, a word for Lyra's insides, and every spelling of a gap but
the one above. Text formatted into it -- a name, a path, a system's own words -- is not checked,
which is why the literal says where it goes.

The body of an internal error is outside these rules. It is written for whoever fixes the compiler,
and a note after it tells the user to report it.
