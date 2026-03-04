# 3/3/2026 4:48pm Prefix Notation Calculator
==================================================
What I know: I remember working with prefix notation in Algorithms class last semester. A stack queue was used to keep track of the order of operations.
Languages: I will be using racket to program the prefix notation calculator.
Additional Notes: The program can be ran in two modes (interactive or batch).
In batch mode it should only output the results (incl. errors).
Overall Flow of The Program:
1.) Prompt the user for an expression
2.) Check if the expression is valid
3.) Check if the user wants to 'quit'
4.) Compute the expression
5.) Return the result.
6.) Save the result to 'History' list
7.) Loop again.

Error Handling: Error message should be prefixed with "Error: "
Expressions:
+ A binary operator that adds together the result of two expressions.
* A binary operator that multiplies together the result of two expressions.
/ A binary operator that divides (integer division) the result of the first expression by the result
of the second. Don’t forget that the user might try to divide by zero. This is an error.
- A unary operator that negates the value of an expression. There is no subtraction. To subtract
we can add a negative number.
$n  A integer value specifying to use the history value corresponding to id n.
any number – a value

Whitespace: Only matters to distinguish the 'tokens'. Therefore only the inital whitespace matters. We don't tokenize first because [operand][number] must be valid.
# 3/3/26 9:45
Stack queue implementation won't work because we have to evaluate while parsing and return (value, remaining input).




