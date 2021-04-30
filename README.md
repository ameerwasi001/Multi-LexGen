# Multi-LexGen
Multi LexGen is an easily integrateable lexer generator that works with multiple languages, currently officially supported languages include Python, Rust, and FSharp(and consequently the dotnet). It has a very intuitive recursive interface like a LL(*) recusrive descent parser generator but it leverages that to create a lexer instead, every rule in a lexer is specified with a regular expression-like interface that can reference any rule including itself. Following is an example of how you'd detect numbers in this Multi-LexGen.
```
digit = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
num = digit+
decimal = num ('.' num)?
```

Now, naturally a critical question for this kind of parser would be how does it handle giving it's tokens names. This is exactly where Data extrators come in, to mark every decimal you can simply say
```
NUM = decimal
```
There can be multiple of these extractors and they are all applied one after another in the given order on the input, and the first one that matches on that input will be returned. Here's how the multiple of the will be handled by the lexer generator.
```
digit = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
num = digit+
decimal = num ('.' num)?

NUM = decimal
PLUS = '+'
MINUS = '-'
MUL = '*'
DIV = '/'
```
This lexer is able to parse any input of following kind.
```
1+2-31.3/1*4
```
and the output will obviously be
```
[NUM:1]
[PLUS:+]
[NUM:2]
[MINUS:-]
[NUM:31.3]
[DIV:/]
[NUM:1]
[MUL:*]
[NUM:4]
```
Even though this can be considered a sufficient solution, there's also another thing we need to discuss, Data Modifiers. Here's an example of how data modifiers can be used to remove whitespace.
```
digit = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
num = digit+
decimal = num ('.' num)?

NUM = decimal
WHITESPACE = '\r' | '\n' | '\t' | ' '
PLUS = '+'
MINUS = '-'
MUL = '*'
DIV = '/'

WHITESPACE ?= _
```
Now the output will remain the same even if the input changes to
```
1+2 - 31.3 / 1 * 4
```
You can also add conditionals here, consider the following
```
digit = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
num = digit+
decimal = num ('.' num)*

NUM = decimal
WHITESPACE = ' '

WHITESPACE ?= _
NUM ?= contains '.' 1 ? FLOAT
NUM ?= contains '.' 3 ? IPV4
NUM ?= contains '.' 0 ? INT
NUM ?= !'Unsupported number of periods found'
```
These conditions applied in the given order for a specific token but no order is guranteed for different input tokens. The above, given this input
```
1 1.1.1.1 1.1
```
will output
```
[INT:1]
[IPV4:1.1.1.1]
[FLOAT:1.1]
```
but if the given output is 
```
1.1.1
```
then it would raise an error*(Error is very much language dependent, in Fsharp or Rust for example it would return an error while an exception would be raised when using Python) because of the last modification like such
```
Unsupported number of periods found
```
The functions such as contains come from a specified helper module from the language being used, the command line tool takes commands with the following syntax, there exists a helper module containing few functions for every language runtime
```
[tool-name] [output-language] [source-file] [output-file-path] [helper-module]
```
functions in these modules should take a token followed by whatever arguments it deems nessacary and always returns a boolean so that a decision on whether it's matched or not can be made. More information is available about integration in the integration section.
# Integration
Comming soon...
