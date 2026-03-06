# Prefix Expression Calculator
====================================
This program implements a prefix notation expression calculator written in Racket.
The calculator reads prefix expressions from the user, evaluates them, and stores results in a history list that can be referenced later.

The program supports both interactive mode and batch mode.

The calculator features the following operators (+ * / -).
* Expressions may contain whitespace
* Oerators may contain whitespace anywhere
* Parentheses are not used
* Extra tokens after a valid expression result in an error

  # File Structure
  =========================================
  mode.rkt - main program
  README.md - documentation for program

  # Implementation Details
  =============================================
  The calculator evaluates expressions using a recursive prefix parser where each parsing
  function returns (value, remaining input)

  1.) Converts input strings into a list of characters
  2.) Recursively parses prefix expressions
  3.) Evaluates expressions during parsing
  4.) Stores results in a history list for $n (n being an integer) for later references in expressions
  
