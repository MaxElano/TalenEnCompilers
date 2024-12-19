# Open questions

## Exercise 4
Happy prefers left-recursive, since it is way more efficient than right-recursive. Left-recursive results in a constant stack-space parser. Whereas right-recursive rules require stack space proportional to the length of the list being parsed. For parsers this will mean that you will go the other way then you would normally go, but it should give no problems, when implemented correctly. This means paying attention to the order of the list and if necessary reversing the list.

## Exercise 10
It matters because the order of executions will be different. Say you execute the recursive call in the middle of your command chain, you will then execute those commands from the recursive call earlier then if you would put the recursive call at the end of your original chain. For syntax it does not matter, as the commands from the recursive call will just be prepended to either the remainder of the original command chain or an empty list if it as the end. It will in both cases be executed, the order is the only thing that matters.