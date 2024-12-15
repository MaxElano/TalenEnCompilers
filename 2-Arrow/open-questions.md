# Open questions

## Exercise 4
Happy prefers left-recursive, since it is way more efficient than right-recursive. Left-recursive results in a constant stack-space parser. Whereas right-recursive rules require stack space proportional to the length of the list being parsed. For parsers this will mean that you will go the other way then you would normally go, but it should give no problems, when implemented correctly. This means paying attention to the order of the list and if necessary reversing the list.

## Exercise 10
