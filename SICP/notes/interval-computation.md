Interval Computations
===

Description:
---
A problem is proposed in Exercises 2.14 - 2.16, that the results computed by different but algebraically equivalent expressions are different.

The cause of this problem is that, when multiple occurences of the same variable are to participate in the calculation, they are treated as independent interval variables rather than holding the same value. So when computing A/A using the procedures created before, we get an interval instead of 1.

I have seen a solution for E2.16 like below:

> If f is a function of intervals I1, I2,... In, then the interval of f(I1, I2,... In) can be calculated as follows:
> 
> Each interval Ii has a lower-bound and upper-bound, for (I1, I2,... In), we get 2^n combinations. Applying each combination to f yields a set of 2^n values. We pick the minimum of these as lower-bound of f, and the maximun as the upper-bound, just like we did in mul-interval.

However, this solution is not correct. For counter example:

f(x) = x^2, and i = [-2, 2].

Reference:
---
[Schemewiki solutions](http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16)

[Interval Computations](http://www.cs.utep.edu/interval-comp/main.html)