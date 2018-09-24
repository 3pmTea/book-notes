

## Pitfalls

* `yytext` is immutable. To parse a string literal with escape sequence support, you should use a buffer to hold the temporary string and copy it to the heap when finished.

## Reference
[Flex document](https://www.cs.princeton.edu/~appel/modern/c/software/flex/flex.html)

[How to implement escape sequences](https://stackoverflow.com/questions/5418181/flex-lex-encoding-strings-with-escaped-characters)