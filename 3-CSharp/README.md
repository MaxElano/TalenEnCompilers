# B3TC Lab 3

This directory contains a compiler for a subset of C#.

The target language is the *Simple Stack Machine* (SSM),
a virtual machine with a graphical simulator.

You can run the graphical simulator by calling `ssm.sh`,`ssm2.sh`,`ssm.bat`, or `ssm2.bat`. Which you should call depends on your OS (`.bat` on windows) and your Java version.
If none of those work, there is also an online version: https://ssm.asraphiel.dev/.
It is made and maintained by an ex-student over at https://github.com/J00LZ/ssmrs, and while we can't promise anything, we've only seen one bug in it, which seems to be fixed now.

## Tasks

1. (0 pt)
    Get familiar with the features the compilers already supports, and the testing framework.

2. (1 pt) Done (Not Fully Tested): Max
    Fix the priorities of the operators. In the starting framework, all
    operators have the same priority. Use the official C# reference to determine the correct order of operations:

    * <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/>
    * <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions>

3. (0.5pt) Done: Max
    In the lexer, discard C# single-line comments.

4. (0.5 pt)
    Make the parser handle both left-associative and right-associative operators.
  
    Again, use the official language reference to determine what the assocativity of each operator should be!

    This means that, e.g.
  
    * `a = b = 1` should be parsed as `a = (b = 1)` (right-associative)
    * `a + b + c` should be parsed as `(a + b) + c` (left-associative)

5. (1 pt)
    Extend the compiler to support a `for` statement.
    In particular, we extend the grammar as such:

    ```bnf
    stat ::=  ...
          |   for ( exprdecls? ; expr ; exprdecls? ) stat
    exprdecls ::= exprdecl | exprdecl , exprdecls
    exprdecl  ::= expr | decl
    ```

    This lets us write (e.g.) the following loop:

    ```csharp
    for(int i, i = 0; i<5; i = i + 1){}
    ```

    Make sure that the generated code behaves as expected.

    Hint: you may find generating code for `for` loops easier if you first translate
    their abstract syntax to use `while` loops (this is called 'desugaring').

6. (2 pt)
    Adapt the code generator such that the declared local variables can be used.

    Hint: you should change the result type for statements in the algebra to be a *pair* of two things:
    the generated code, *plus* a list of any variables declared.

    Remember that in C# local variables can be declared anywhere in a method body,
    but they *always* must be declared *before use*.
    So, in a sequence of statements, the environment passed to a statement must contain all variables declared before it.

7. (1.0 pt)
    Change the code generator for the logical operators, so that they are computed lazily, as is usual in C#.

    In other words, the right operand should only be evaluated if necessary to determine the result.

8. (1.5 pt)
    Add the possibility to 'call a method with parameters' to the syntax of expressions,
    and add the possibility to deal with such calls to the rest of the compiler.

    Make sure that the parameters can be used within the function body.

    Hint: In the codeAlgebra, you have to change the result types.
    You need to pass around an environment that contains the addresses of parameters.

9. (0.5pt)
    Add a special case for a method call to `print` which,
    instead of jumping to the label "`print`",
    evaluates its argument(s) and does `TRAP 0` for each argument.

    The command `TRAP 0` will pop and print the topmost element from the stack.

    For example,

    ```csharp
    print(2+3)
    ```

    should be compiled to

    ```
    LDC 2
    LDC 3
    ADD
    TRAP 0
    ```

    and

    ```csharp
    print(1,2)
    ```

    should first print `1`, and then print `2`.

10. (1 pt)
    Extend the code generator such that methods can have a result.

    You may choose whether you want to pass the result via register
    or via the stack.

For the next few exercises, implement the analyses in `src/Analysis.hs` using an algebra.
You may choose whether you implement them all in one algebra, or using seperate 
algebra's for each of the analyses. Modify `src/Main.hs` accordingly.

11. (1 pt)
    Modify the compiler to fail at compile-time if the input contains
    any references to undefined variables.

    (This is called *scope-checking*)

12. (0.5 pt)
    Modify the compiler to fail at compile-time if the input contains
    any assignments to variables of the wrong type.

    (This is called *type-checking*)

13. (bonus, 0.5 pt)
    Modify the compiler to fail at compile-time if the input contains
    any function calls with the wrong number of arguments.

14. (bonus, 0.5 pt)
    Modify the compiler to fail at compile-time if the input contains
    any function calls with arguments of the wrong type.
    
15. (bonus, 1 pt)
    Modify the code generator such that declared member variables can be used.

    Our C# programs consist of exactly one `Class`,
    which means that these are global variables.

## Compiler Notes

### Parsing

The file `src/Parser.hs` contains a parser for out subset of C#,
impemented using the `uu-tc` library.

#### Beware `greedyChoice`!

The keyword lexer must occur before `lexLowerId`, because a keyword such as "`class`"
or a type such as "`int`" could also be interpreted as lowercase identifiers.

For similar reasons, `OpLeq` must occur before `OpLt` in `AbstratSyntax.Operator`,
otherwise the string "`<=`" might be interpeted as the operator `<` followed by the operator `=`
rather than as a single operator.

### Simple Stack Machine

To simplify generation of target code for the simple stack machine, 
the file `src/SSM.hs` defines an abstract syntax tree for SSM code.

This allows the generation of SSM code to be split into two parts:

1. Generation of SSM abstract syntax (complex code, but type-checked by Haskell)
2. Printing out of arbitrary SSM abstract syntax (simple, repetitive code defined alongside the AST)

The structure of an SSM program is simple -- it is a list of instructions.

### Acknowledgements

This assignment is heavily inspired and to a large extent copied from an
assignment Johan Jeuring has been using.

## Sumbission Instructions

* Make sure your program compiles.
  You can test it with `cabal run`.

* Include \emph{useful} comments in your code.
  Do not paraphrase the code,
  but describe the general structure, special cases, preconditions, invariants, etc.

* Try to write readable and idiomatic Haskell.
  Style influences the grade!

    The use of existing higher-order functions (e.g. `map`, `foldr`, `filter`, `zip`) is encouraged.
    The use of existing libraries is allowed (as long as the program still compiles with the above invocation).
    If you want to use a package that isn't listed in the `P3-CSharp.cabal` file yet, check with the teachers first (we will probably approve).

* Copying solutions from the internet is not allowed.

* You may work alone or with one other person.
  This does not have to be the same team as in previous assignments. 

    Please include the full names and student numbers of *all* team members on a header at the top of the Main file.

* Blackboard will be used.

* Textual answers to tasks can either:

    * Be included as comments in a source file.
    * Be submitted as text files.

    Microsft Word documents are not accepted!

* Please run `cabal clean` (or similar) prior to submission, to remove the generated files.

    Also remove the `.jar` files which are big and waste bandwidth

    Hint: you may be interested the `git clean` command

* Submit a buildable project in a `.zip` file.
