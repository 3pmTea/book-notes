How to test and play with the code
---
In [DrRacket.exe](https://racket-lang.org/), select File -> Open, locate these files, open one of them, then click "Run".

*Chapter.2.pict-lang.rkt* requires the package 'sicp' to run, which can be installed by following [these steps](https://docs.racket-lang.org/sicp-manual/index.html).

The code in *Chapter.4c.rkt* can't run directly in DrRacket. A nondeterministic interpreter, which supports "amb", is required. To test these code, load "include/nondeterministic.rkt" in DrRacket, call (driver-loop), and copy & paste the procedure definitions into the input area.

Some notes:
---
* I use *\<func-name>-x.xx* if *\<func-name>* is a built-in name of Racket.
* The chapters on NLP, non-deterministic interpreter and logic programming are left undone. I'll finish them some time later.

My contributions to [schemewiki](http://community.schemewiki.org/?sicp-solutions):
---
[E2.42](http://community.schemewiki.org/?sicp-ex-2.42),
[E3.30](http://community.schemewiki.org/?sicp-ex-3.30),
[E3.67](http://community.schemewiki.org/?sicp-ex-3.67),
[E4.7](http://community.schemewiki.org/?sicp-ex-4.7),
[E4.8](http://community.schemewiki.org/?sicp-ex-4.8),
[E4.42](http://community.schemewiki.org/?sicp-ex-4.42),
