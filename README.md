# Mini-Compiler-For-C

This is a compiler for while and do-while constructs of the C language written in Python. It uses <a href='https://www.dabeaz.com/ply/'> PLY </a> (Python Lex-Yacc) which is a lex and yacc implementation for Python.

The grammar written for this version of C handles the following language components -
* Iterative constructs - while and do-while
* Arithmetic and logical expressions
* Declarative statements (variable declarations of primitive and compound types)
* Function calls

The Lexical Analysis phase is designed to recognize syntactic units of C. This includes keywords, operators, C style identifiers, signed and unsigned integral and floating point values, as well as real numbers in scientific notation. Ply Lex provides support for defining keywords and reserved words as tokens. Other tokens such as identifiers are specified by means of regular expressions.

The tokens returned by the Lexical Analysis phase are then parsed using the grammar written for this version of C. In this phase, the tokens are parsed to match the appropriate C constructs. We add error checking mechanisms to the parsing process i.e. a global error production is defined to notify the user of errors with their corresponding line numbers.


Find design details in the "Compiler Documentation" file. This file documents the design strategy and implementation details of 
* The Symbol Table
* Abstract Symbol Tree Construction 
* Intermediate Code Generation
* Code Optimisation Mechanisms
* Error Handling Mechanisms

Run the "compiler.py" file to experiment with the compiler.
