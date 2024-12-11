# The Arrow Language

The concrete syntax of the Arrow language is given by the following grammar with start
symbol Program:

```
Program → Rule*
Rule    → Ident -> Cmds .
Cmds    → ε | Cmd (, Cmd )*
Cmd     → go | take | mark | nothing
        | turn Dir
        | case Dir of Alts end
        | Ident
Dir     → left | right | front
Alts    → ε | Alt (; Alt)*
Alt     → Pat -> Cmds
Pat     → Empty | Lambda | Debris | Asteroid | Boundary | _
```

A program is a sequence of rules. Think of rules as procedures: A name is bound to a
sequence of commands. Rules are terminated by a period.

Commands are separated by commas. There is a fixed number of commands. These
are instructions for Arrow. Informally, `go` means “move in the current direction if
possible”, `take` means “pick up whatever is here”, `mark` means “leave a lambda in the
current spot”, `nothing` means “do nothing”, `turn` takes a direction and causes Arrow to
turn left or right. The `case` command takes a direction and performs a sensor reading
in that direction. Depending on what is sensed, different actions may be taken. Finally,
another rule can be invoked by naming it.

In a `case` construct, multiple alternatives can be provided (separated by semicolons)
that map patterns to rules. Patterns correspond to the things that can be located in a
certain position, and there is a catch-all pattern called `_`.
Note that unlike in Haskell, case expressions are terminated by an `end` keyword.
The lexical syntax of a program is described as follows: the program text consists of
a (possibly space-separated) sequence of tokens.

```
Token → -> | . | , | go | take | mark | nothing | turn | case | of | end
      | left | right | front | ; | Empty | Lambda | Debris | Asteroid 
      | Boundary | _ | Ident
Ident → (Letter | Digit | + | -)+
```

A token is either symbolic, a command keyword, a pattern keyword, or an identifier. It
is implicitly understood that an Ident must not be any of the keyword tokens and must
not be directly followed by another character that could occur in an identifier.
Furthermore, comments may occur in programs between tokens. These are introduced
by `--` and extend to the end of the line, like in Haskell.

## Space
Arrow lives on a rectangular board that we call “space” and represent using a finite map
(dictionary), from the module Data.Map:
```
type Space = Map Pos Contents
type Size = Int
type Pos = (Int, Int)
data Contents = Empty | Lambda | Debris | Asteroid | Boundary
```
We assume that there always is a rectangular area of positions with non-negative row-
and column-coordinates contained in the finite map, including position (0, 0). 
Functions can use `findMax` to find the maximum key and hence the maximum position in a given space.
We define an input format for spaces where contents are represented by characters:

  contents    character
  empty       .
  lambda      \
  debris      %
  asteroid    O
  boundary    #

We specify the format by example:
(7,7)
........
....%...
..%%%%..
....%%%.
...%%%..
....%.%%
....%%%%
........
The first line contains the maximum valid row-column-coordinate for the board. Here,
we thus have a space with 8 rows and 8 columns. The rows are then specified line by
line, starting with row 0 end ending with row 7. The example space contains a field of
debris, but otherwise just empty space.
A parser for the input format is included in the starting framework in [src/Interpreter.hs](src/Interpreter.hs).
