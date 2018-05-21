Random Number Generators with a coin
===

Related:
---
### Exercise 5.1-2
Describe an implementation of the procedure RANDOM(a, b) that only makes calls to RANDOM(0, 1). What is the expected running time of your procedure, as a function of a and b?

### Exercise 5.1-3
Suppose that you want to output 0 with probability 1/2 and 1 with probability 1/2. At your disposal is a procedure BIASED-RANDOM, that outputs either 0 or 1. It outputs 1 with some probability p and 0 with probability 1-p, where 0 < p < 1, but you do not know what p is. Give an algorithm that uses BIASED-RANDOM as a subroutine, and returns an unbiased answer, returning 0 with probability 1/2 and 1 with probability 1/2. What is the expected running time of your algorithm as a function of p?

Reference:
---
[Stackoverflow](https://stackoverflow.com/questions/13209162/creating-a-random-number-generator-from-a-coin-toss)