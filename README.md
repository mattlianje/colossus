# yavanna
```
/**
  * Yavanna and her corresponding ARM-6
  */
func add(x: int, y: int) -> int {
    return (x + y);
}

/*
.global add
add:
    PUSH {lr}
    ADD r0, r0, r1
    POP {pc}
*/
```
- A small, unoptimizing compiler for my synthetic language - `yavanna`. 
It targets ARMv6-ish, features LL(2) grammar and is described in EBNF.

- This library crate is mainly for self-study of the 
[Green Dragon Book](https://en.wikipedia.org/wiki/Principles_of_Compiler_Design), 
and is named after [Yavanna](https://lotr.fandom.com/wiki/Yavanna) the Valar of fruits, as
a tip of the hat to Dr. Richard Hipp's [lemon C parser](https://sqlite.org/src/doc/trunk/doc/lemon.html).

<p align="center">
  <img src="images/yavanna.png" width="450" alt="Yavanna">
</p>


