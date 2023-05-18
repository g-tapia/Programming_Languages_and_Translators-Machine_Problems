# Racket
Welcome to this Racket repository where you will find three unique lab assignments designed to challenge your understanding and implementation skills of different computing concepts. The labs are as follows:

## Implementing Library Functions from Scratch (MP1): 
This lab will give you hands-on experience in reimplementing some common library functions. This will help you understand the underlying mechanism and performance considerations of these functions.

## Implementing a Metacircular Evaluator (MP2): 
A step up from the first lab, this one will introduce you to the concept of metacircular evaluators

### Metacircular Evaluator

   In the world of programming languages, an evaluator is a system or routine that interprets and executes instructions written in a programming language. A metacircular evaluator, on the other hand, is a special type of evaluator that is implemented in the language it is intended to evaluate.

  The term "metacircular" refers to the cyclic nature of this concept where the implementing language and the implemented language are the same. Metacircular evaluators are an interesting concept because they allow us to understand the semantics of a language from within itself. Building one offers a deep and practical understanding of how language features interact and behave.
The purpose of a metacircular evaluator is to provide a framework for understanding how an interpreter or compiler works. It allows you to see how the fundamental evaluation processes of a programming language can be implemented using the same language constructs.

Here's a high-level overview of how a metacircular evaluator works:

Parsing: The evaluator takes a Racket expression as input and parses it to create an internal representation of the expression called an abstract syntax tree (AST). The AST captures the structure and meaning of the expression.

Evaluation: The evaluator traverses the AST and evaluates each expression based on the rules of the Racket language. It starts with the outermost expression and recursively evaluates nested expressions.

Environment: The evaluator maintains an environment, which is a data structure that keeps track of variables and their corresponding values. When the evaluator encounters a variable in an expression, it looks up the variable's value in the environment.

Special Forms: The evaluator handles special forms, such as conditionals (if) and function definitions (define). These forms have special evaluation rules that differ from regular function application.

Function Application: When the evaluator encounters a function call, it evaluates the arguments of the function and then applies the function to those arguments. The evaluation of function calls can be recursive, allowing nested function calls.

Return Values: The evaluator returns the value of the evaluated expression. This value can be used in further evaluations or displayed as the result of the program.

The purpose of a metacircular evaluator goes beyond just evaluating expressions. It serves as a tool for understanding the core concepts of a programming language, such as scoping, variable binding, and function application. By implementing an evaluator in the same language it evaluates, you gain insight into how the language itself works and can experiment with modifying its behavior.

Metacircular evaluators are often used in educational settings to teach students about programming languages and interpreters. They provide a hands-on approach to understanding the internals of a language and can be a stepping stone to building more advanced language processors like interpreters and compilers.

## Implementing a Racket Interpreter (MP3): 
This lab goes a level deeper,building a Racket interpreter. This is a great way to understand how programming languages work at their core.

### Interpreter

An interpreter is a component of a computer system that directly executes instructions written in a programming or scripting language without requiring them to be previously compiled into a machine language program. In other words, an interpreter translates high-level instructions into an intermediate form, which it then executes.

Implementing an interpreter involves understanding the structure and syntax of a programming language and being able to parse and execute those instructions. Creating an interpreter gives you an in-depth understanding of a programming language's semantics and the low-level details of its execution model.

In this repository, the third lab involves creating a simple interpreter for the Racket language. This exercise is a great way to deeply understand how Racket works and will enhance your overall understanding of interpreters and language design.


