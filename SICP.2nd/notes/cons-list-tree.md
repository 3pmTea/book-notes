Cons, List and Tree in Scheme
===

If I want to construct a complete binary tree with 4 node (eg. <code>((1 2) (3 4))</code> ) I should use <code>(list (list 1 2) (list 3 4))</code>, instead of <code>(cons (list 1 2) (list 3 4))</code> or <code>(cons (cons 1 2) (cons 3 4))</code>, which yield <code>((1 2) 3 4)</code> and <code>((1 . 2) 3 . 4)</code> respectively. Why?

Reference:
---
[How to represent binary tree?](https://stackoverflow.com/questions/34784351/how-to-represent-binary-tree)

