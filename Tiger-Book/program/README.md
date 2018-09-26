

## Pitfalls

* `yytext` is immutable. To parse a string literal with escape sequence support, you should use a buffer to hold the temporary string and copy it to the heap when finished.

* you should always use left recursion, because it can parse a sequence of any number of elements with bounded stack space. Right recursion uses up space on the Bison stack in proportion to the number of elements in the sequence, because all the elements must be shifted onto the stack before the rule can be applied even once. See [The Bison Parser Algorithm](https://www.gnu.org/software/bison/manual/bison.html#Algorithm), for further explanation of this. 

## Reference
[Flex Document](https://www.cs.princeton.edu/~appel/modern/c/software/flex/flex.html)

[Bison Document](https://www.gnu.org/software/bison/manual/bison.html)

[How to Implement Escape Sequences](https://stackoverflow.com/questions/5418181/flex-lex-encoding-strings-with-escaped-characters)

[Lex and YACC Primer/HOWTO](http://www.tldp.org/HOWTO/Lex-YACC-HOWTO.html)
