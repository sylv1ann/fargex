# Project description:
---
Fargex is a program which executes the following conversions:
* RegEx -> Finite State Machine conversion.
* Finite State Machine -> RegEx conversion.
## How to run fargex:
---
There's only one `cabal run` command which is needed to run fargex.

## Dataset for testing:
Testing dataset can be found as `dataset.yml` file above.
Example: 
```
Input RegEx: a+b 
FSM [(4,"$",[0,2]),(0,"a",[1]),(2,"b",[3]),(1,"$",[5]),(3,"$",[5])] [4] [5]
```
![FSM result](http://www.ms.mff.cuni.cz/~bujkov/neproc/fargex/fsm.png)
```
Input FSM: FSM [(4,"$",[0,2]),(0,"a",[1]),(2,"b",[3]),(1,"$",[5]),(3,"$",[5])] [4] [5]
($a$)+($b$)
```

## Documentation: 
The Haddock documentation is available [here] 

[here]: <http://www.ms.mff.cuni.cz/~bujkov/neproc/fargex/documentation/>
