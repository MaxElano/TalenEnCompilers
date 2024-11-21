# B3TC Lab 1

This directory contains an un-finished parser for a subset of the [iCalendar file format]

[iCalendar file format]: http://en.wikipedia.org/wiki/ICalendar

## Tasks

1. (0 pt)
    Read through the implementation of the parser, and the grammar of the [iCal datetime language](ical-datetime-spec.md)

2. (6 pt)
    Implement the function

    ```haskell
    parseDateTime :: Parser Char DateTime
    ```

    that can parse a single date and time value.
    
    Hint: you may find this task easier if you first define parsers for `Date`,
    `Time`, `Hour`, etc.

3. (2pt)
    Implement the function

    ```haskell
    run :: Parser a b -> [a] -> Maybe b
    ```

    that applies a parser to an input sequence.

    `run` should return the first result that is a complete parse,
    i.e. where the remaining list of input symbols is empty.

    `run` should return `Nothing` iff no such result exists.

4. (2 pt)
    Implement the function

    ```haskell
    printDateTime :: DateTime -> String
    ```

    that turns a date and time value back into an iCal datetime string.

5. (0 pt)
    Test your code.

    Your implementations of `parseDateTime` and `printDateTime` shoudld be *inverses* of each other,
    i.e. for any value `dt` of type `DateTime`, and any iCal datetime string `s`,

    ```haskell
    run parseDateTime (printDateTime dt) == Just dt
    ```
    and
    ```haskell
    printDateTime <$> run parseDateTime s == Just s
    ```

    You can use `ghci` for manual testing, e.g.

    ```ghci
    *Main> parsePrint "19970610T172345Z"
    Just "19970610T172345Z"
    *Main> parsePrint "19970715T040000Z"
    Just "19970715T040000Z"
    *Main> parsePrint "19970715T40000Z"
    Nothing
    *Main> parsePrint "20111012T083945"
    Just "20111012T083945"
    *Main> parsePrint "20040230T431337Z"
    Just "20040230T431337Z"
    ```
    
    Hint: the last example demonstrates that the your parser should accept
    certain non-sensical timestamps, as required by the grammar. For example,
    hours can only range from 0 to 23, but the grammar generates all
    2-digit natural numbers.

6. (3 pt)
    Implement the function

    ```haskell
    checkDateTime :: DateTime -> Bool
    ```

    that verifies that a DateTime represents a valid date and time.

    Any 4-digit value should be accepted as a valid year, and years in "BC"
    (Before Christ) are ignored. Valid months are in the range 1-12, and valid
    days are in the range 1-28, 1-29, 1-30 or 1-31, depending on the month.
    Valid times are those where the hour is in the range 0-23 and minute and
    seconds are in the range 0-59.

    Read about the [Gregorian Calendar] for more information about the number of
    days per month. Make sure that you handle [leap years] in the correct way!

    [Gregorian Calendar]: https://en.wikipedia.org/wiki/Gregorian_calendar
    [leap years]: https://en.wikipedia.org/wiki/Leap_year

    e.g.

    ```ghci
    *Main> let parseCheck s = checkDateTime <$> run parseDateTime s
    *Main> parseCheck "19970610T172345Z"
    Just True
    *Main> parseCheck "20040230T431337Z"
    Just False
    *Main> parseCheck "20040229T030000"
    Just True
    ```

    Tip: Write functions to work with the datatypes produced by the parser, even for the
    simple ones. For example, if you need to subtract two values of Year, define a function
    `subYears :: Year -> Year -> Year`.

    Tip: You are allowed to use time libraries

7. (8 pt)
    Define Haskell datatypes (or type synonyms) to describe the abstract syntax
    of an [iCal calendar](ical-calendar-spec.md)
    
    Call the type for a whole iCalendar file `Calendar`.

    Hint: The abstract syntax does not need to have the same structure as the concrete
    syntax. Read the informal explanation of the format several times, and think about
    the best way to represent the calendar. Which elements of the concrete syntax are
    important, and which elements are irrelevant?

8. (8 pt)
    Implement the datatype `Token`, and the two functions

    ```haskell
    lexCalendar :: Parser Char [Token]
    parseCalendar :: Parser Token Calendar
    ```

    which lex and parse the calendars, respectively.

9. (3 pt)
    Implement the function

    ```haskell
    printCalendar :: Calendar -> String
    ```

    that generates a string representation from an abstract `Calendar` object.

    Each line should be at most 42 characters long. If a line is too large, you
    should split it as in the multiline example.

10. (0 pt) Test your code

    Your implementation should satisfy the property that,
    for any value `c` of type `Calendar`,

    ```haskell
    recognizeCalendar (printCalendar c) == Just c
    ```

    The printer may change the layout or order of properites,
    so is **not** required that for every legal calendar string `s`,

    ```haskell
    printCalendar (recognizeCalendar s) == Just s   -- not required
    ```

    Here is one possible result of printing `examples/bastille.ics` -- note the changes in layout:

    ```
    BEGIN:VCALENDAR
    PRODID:-//hacksw/handcal//NONSGML v1.0//EN
    VERSION:2.0
    BEGIN:VEVENT
    SUMMARY:Bastille Day Party
    UID:19970610T172345Z-AF23B2@example.com
    DTSTAMP:19970610T172345Z
    DTSTART:19970714T170000Z
    DTEND:19970715T040000Z
    END:VEVENT
    END:VCALENDAR
    ```

11. (3 pt)
    Implement the functions

    ```haskell
    countEvents :: Calendar -> Int
    findEvents :: DateTime -> Calendar -> [Event]
    checkOverlapping :: Calendar -> Bool
    timeSpent :: String -> Calendar -> Int
    ```

    which (respectively):

    * Count the total number of events in a calendar.
    * Find events which are happening at a given date and time.
      An event should not be counted if the searched time matches exactly the end time.
    * Check whether or not a calendar contains overlapping events.
    * Count the total number of minutes spent for events with a given summary.

    For this exercise, ignore the `timeutc` flag, and assume that the local time
    is UTC.

    Hint: You can choose whatever form of computation you find easiest. In
    particular, you can choose to define your own datatypes, and whether you
    want to define one or multiple functions to collect the data.

## Acknowledgements

This assignment was developed by Jeroen Bransen, João Pizani, Trevor McDonell,
Alejandro Serrano, David van Balen, and Lawrence Chonavel.

## Sumbission Instructions

* Make sure your program compiles.
  Please do not submit attempts which don’t even compile.

* Include **useful** comments in your code.
  Do not paraphrase the code,
  but describe the general structure, special cases, preconditions, invariants, etc.

* Try to write readable and idiomatic Haskell.
  Style influences the grade!

    The use of existing higher-order functions (e.g. `map`, `foldr`, `filter`, `zip`) is encouraged.
    The use of existing libraries is allowed (as long as the program still compiles).
    If you want to use a package that isn't listed in the `P1-icalendar.cabal` file yet, check with the teachers first (we will probably approve).

* You may work alone or with one other person (preferred).

  Please include the full names and student numbers of *all* team members in a comment at the top of the Main file.

* Submit through Blackboard

