# iCal datetime language specification

Here is a grammar for the lanaguage of *iCal datetime* strings:

```
datetime  ::= date datesep time
date      ::= year month day
time      ::= hour minute second timeutc
year      ::= digit digit digit digit
month, day, hour , minute, second
          ::= digit digit
timeutc   ::= ε | Z
digit     ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
datesep   ::= T 
```

No whitespace is allowed anywhere in an iCal datetime string.
