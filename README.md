# Whitespace Interpreter

[Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)) is an esoratic, imperative, stack-based programming language, including features such as subroutines.
See the original haskell imprlementation along with the language specification [here](https://github.com/wspace/whitespace-haskell).
Whitespace uses only three characters, all other characters may be used for comments. The interpreter ignores them.
* <space> or " " (ASCII 32)
* <tab> or "\t" (ASCII 9)
* <line-feed> or "\n" (ASCII 10)

## IMP (Instruction Modification Parameter)
Each command in whitespace begins with an Instruction Modification Parameter (IMP):
* <space>: Stack Manipulation
* <tab><space>: Arithmetic
* <tab><tab>: Heap Access
* <tab><line-feed>: Input/Output
* <line-feed>: Flow Control

## Data
There are two types of data a command may be passed - numbers and labels.

### Numbers
* Numbers begin with a [sign] symbol. The sign symbol is either <tab> -> negative, or <space> -> positive.
* Numbers end with a terminal symbol: <line-feed>.
* Between the sign symbol and the terminal symbol are binary digits <space> -> binary-0, or <tab> -> binary-1.
* A number expression <sign><terminal> will be treated as zero.
* The expression of just <terminal> should throw an error.

### Labels
* Labels begin with any number of <tab> and <space> characters.
* Labels end with a terminal symbol: <line-feed>.
* Unlike with numbers, the expression of just <terminal> is valid.
* Labels must be unique.
* A label may be declared either before or after a command that refers to it.

## Input/Output
As stated earlier, there commands may read data from input or write to output.

### Parsing Input
* Whitespace will accept input either characters or integers. Due to the lack of an input stream mechanism, the input will be passed as a string to the interpreter function.
* Reading a character involves simply taking a character from the input stream.
* Reading an integer involves parsing a decimal or hexadecimal number from the current position of the input stream, up to and terminated by a line-feed character.
* The original implementation being in Haskell has stricter requirements for parsing an integer.

### Writing Output
* For a number, append the output string with the number's string value.
* For a character, simply append the output string with the character.

## Commands
Notation: n specifies the parameter, <number> or <label>.

Errors should be thrown for invalid numbers, labels, and heap addresses, or if there are not enough items on the stack to complete an operation (unless otherwise specified). In addition, an error should be thrown for unclean termination.

### IMP <space> - Stack Manipulation
* <space> (number): Push n onto the stack.
* <tab><space> (number): Duplicate the nth value from the top of the stack.
* <tab><line-feed> (number): Discard the top n values below the top of the stack from the stack. (For n<0 or n>=stack.length, remove everything but the top value.)
* <line-feed><space>: Duplicate the top value on the stack.
* <line-feed><tab>: Swap the top two value on the stack.
* <line-feed><line-feed>: Discard the top value on the stack.

### IMP <tab><space> - Arithmetic
* <space><space>: Pop a and b, then push b+a.
* <space><tab>: Pop a and b, then push b-a.
* <space><line-feed>: Pop a and b, then push b*a.
* <tab><space>: Pop a and b, then push b/a*. If a is zero, throw an error.
*Note that the result is defined as the floor of the quotient.
* <tab><tab>: Pop a and b, then push b%a*. If a is zero, throw an error.
*Note that the result is defined as the remainder after division and sign (+/-) of the divisor (a).

### IMP <tab><tab> - Heap Access
* <space>: Pop a and b, then store a at heap address b.
* <tab>: Pop a and then push the value at heap address a onto the stack.

### IMP <tab><line-feed> - Input/Output
* <space><space>: Pop a value off the stack and output it as a character.
* <space><tab>: Pop a value off the stack and output it as a number.
* <tab><space>: Read a character from input, a, Pop a value off the stack, b, then store the ASCII value of a at heap address b.
* <tab><tab>: Read a number from input, a, Pop a value off the stack, b, then store a at heap address b.

### IMP <line-feed> - Flow Control
* <space><space> (label): Mark a location in the program with label n.
* <space><tab> (label): Call a subroutine with the location specified by label n.
* <space><line-feed> (label): Jump unconditionally to the position specified by label n.
* <tab><space> (label): Pop a value off the stack and jump to the label specified by n if the value is zero.
* <tab><tab> (label): Pop a value off the stack and jump to the label specified by n if the value is less than zero.
* <tab><line-feed>: Exit a subroutine and return control to the location from which the subroutine was called.
* <line-feed><line-feed>: Exit the program.

