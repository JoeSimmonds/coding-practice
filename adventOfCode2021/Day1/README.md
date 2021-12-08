# Advent of code Day 1
[Coded for MIT/GNU scheme](https://groups.csail.mit.edu/mac/projects/scheme/)

[Day 1 instructions](https://adventofcode.com/2021/day/1)

## input data
in the `input.txt` file

## libs
uses [SCMUnit](https://github.com/nicholasrussell/scmUnit)

## Running the code
```
scheme --quiet --load "increasing.scm" --eval "(main)" --eval "(exit)"
```

## Running the tests
```
scheme --quiet --load "increasing.scm" --eval "(test)" --eval "(exit)"
```

You will see loads of this message which can be ignored :
```
;Warning: Cannot definitively capture errors if standard-error-hook is bound.
```

These are an artefact of the tyesting llibrary and I haven't been able to get rid of them, if these are bothersome you can pipe the output through grep like this
```
scheme --quiet --load "increasing.scm" --eval "(test)" --eval "(exit)" | grep -v -e ';Warning: Cannot definitively capture errors:'
```

## Things I learnt here
 - Surprisingly Scheme does have a unit testing library.
 - Scheme has no property based testing library.
 - Writing one would be hard.
 - Terse syntax simply means that valid programs are near neighbours.
 - Put another way: when you make a mistake your chances of accidentally hitting on a valid but incorrect program are way higher in lisp like languages.
 - The compiler error messages suck.
 - testing behaviours is much easier to grok when there are no classes to provide a misleading unit boundary.
 - TDD and property based testing are not bosom buddies, a property often expresses quite a lot of the complexity.
