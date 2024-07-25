# Whitespace Interpreter

[Whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)) is an esoratic, imperative, stack-based programming language, including features such as subroutines.
See the original haskell imprlementation along with the language specification [here](https://github.com/wspace/whitespace-haskell).
Whitespace uses only three characters, all other characters may be used for comments. The interpreter ignores them.
* <kbd>space</kbd> or " " (ASCII 32)
* <kbd>tab</kbd> or "\t" (ASCII 9)
* <kbd>line-feed</kbd> or "\n" (ASCII 10)

## IMP (Instruction Modification Parameter)
Each command in whitespace begins with an Instruction Modification Parameter (IMP):
* <kbd>space</kbd>: Stack Manipulation
* <kbd>tab</kbd><kbd>space</kbd>: Arithmetic
* <kbd>tab</kbd><kbd>tab</kbd>: Heap Access
* <kbd>tab</kbd><kbd>line-feed</kbd>: Input/Output
* <kbd>line-feed</kbd>: Flow Control

## Data
There are two types of data a command may be passed - numbers and labels.

### Numbers
* Numbers begin with a [sign] symbol. The sign symbol is either <kbd>tab</kbd> -> negative, or <kbd>space</kbd> -> positive.
* Numbers end with a terminal symbol: <kbd>line-feed</kbd>.
* Between the sign symbol and the terminal symbol are binary digits <kbd>space</kbd> -> binary-0, or <kbd>tab</kbd> -> binary-1.
* A number expression [sign][terminal] will be treated as zero.
* The expression of just [terminal] should throw an error.

### Labels
* Labels begin with any number of <kbd>tab</kbd> and <kbd>space</kbd> characters.
* Labels end with a terminal symbol: <kbd>line-feed</kbd>.
* Unlike with numbers, the expression of just [terminal] is valid.
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
Notation: n specifies the parameter, [number] or [label].

Errors should be thrown for invalid numbers, labels, and heap addresses, or if there are not enough items on the stack to complete an operation (unless otherwise specified). In addition, an error should be thrown for unclean termination.

### IMP <kbd>space</kbd> - Stack Manipulation
* <kbd>space</kbd> (number): Push n onto the stack.
* <kbd>tab</kbd><kbd>space</kbd> (number): Duplicate the nth value from the top of the stack.
* <kbd>tab</kbd><kbd>line-feed</kbd> (number): Discard the top n values below the top of the stack from the stack. (For n<0 or n>=stack.length, remove everything but the top value.)
* <kbd>line-feed</kbd><kbd>space</kbd>: Duplicate the top value on the stack.
* <kbd>line-feed</kbd><kbd>tab</kbd>: Swap the top two value on the stack.
* <kbd>line-feed</kbd><kbd>line-feed</kbd>: Discard the top value on the stack.

### IMP <kbd>tab</kbd><kbd>space</kbd> - Arithmetic
* <kbd>space</kbd><kbd>space</kbd>: Pop a and b, then push b+a.
* <kbd>space</kbd><kbd>tab</kbd>: Pop a and b, then push b-a.
* <kbd>space</kbd><kbd>line-feed</kbd>: Pop a and b, then push b*a.
* <kbd>tab</kbd><kbd>space</kbd>: Pop a and b, then push b/a*. If a is zero, throw an error.
*Note that the result is defined as the floor of the quotient.
* <kbd>tab</kbd><kbd>tab</kbd>: Pop a and b, then push b%a*. If a is zero, throw an error.
*Note that the result is defined as the remainder after division and sign (+/-) of the divisor (a).

### IMP <kbd>tab</kbd><kbd>tab</kbd> - Heap Access
* <kbd>space</kbd>: Pop a and b, then store a at heap address b.
* <kbd>tab</kbd>: Pop a and then push the value at heap address a onto the stack.

### IMP <kbd>tab</kbd><kbd>line-feed</kbd> - Input/Output
* <kbd>space</kbd><kbd>space</kbd>: Pop a value off the stack and output it as a character.
* <kbd>space</kbd><kbd>tab</kbd>: Pop a value off the stack and output it as a number.
* <kbd>tab</kbd><kbd>space</kbd>: Read a character from input, a, Pop a value off the stack, b, then store the ASCII value of a at heap address b.
* <kbd>tab</kbd><kbd>tab</kbd>: Read a number from input, a, Pop a value off the stack, b, then store a at heap address b.

### IMP <kbd>line-feed</kbd> - Flow Control
* <kbd>space</kbd><kbd>space</kbd> (label): Mark a location in the program with label n.
* <kbd>space</kbd><kbd>tab</kbd> (label): Call a subroutine with the location specified by label n.
* <kbd>space</kbd><kbd>line-feed</kbd> (label): Jump unconditionally to the position specified by label n.
* <kbd>tab</kbd><kbd>space</kbd> (label): Pop a value off the stack and jump to the label specified by n if the value is zero.
* <kbd>tab</kbd><kbd>tab</kbd> (label): Pop a value off the stack and jump to the label specified by n if the value is less than zero.
* <kbd>tab</kbd><kbd>line-feed</kbd>: Exit a subroutine and return control to the location from which the subroutine was called.
* <kbd>line-feed</kbd><kbd>line-feed</kbd>: Exit the program.

