# Haskell Advent of Code 2023
My solutions for Advent of Code 2023 in Haskell.  Here's some goals I
set to make the most out of it:

1. No looking up solutions/discussing with others before completion.
2. No use of libraries outside of the [GHC bootstrap
  libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html)
  for the solutions.
3. If time permits, use
  [Criterion](https://hackage.haskell.org/package/criterion) to
  benchmark and improve solutions.
4. No unsafe Haskell.

The reason for (2) is that many online competitive programming sites
that support Haskell (CodeForces, Google Code Jam) do not have
libraries beyond the bootstrap list.  Plus, there's already a wealth
of competitive libraries in there, such as `bytestring`, `text`,
`parsec`, `containers` (which contains maps, sets, int sets, graphs,
sequences) and more.

I might do a full writeup after my semester is over, but here's my
rough procedure on how to tackle the problems.

1. Write the most naive thing that could possibly work.  If it works,
   submit the answers!
2. Focus on algorithmic improvements.  I use a combination of
   techniques:
   - use equational reasoning to fuse folds, traversals
   - use more efficient data structures
3. Focus on empirical improvements.  I make heavy use of Criterion,
   though [AutoBench](https://github.com/mathandley/AutoBench) seems
   interesting.
   - manually inlining helper functions and equational reasoning, tail
     recursion
   - convert `foldr` to `foldl` when possible
   - using strict versions of functions, bang patterns
   - explicit type annotations
   - faster types: `Int` instead of `Integer`, `ByteString` instead of
     `String`, `Sequence` or `Vector` instead of `List`

Of course with (3) one could continue shaving off more and more time,
though these heuristics in practice have given me most of the gains.

## Writing Haskell for AoC quickly
AoC is all about solving the problem quickly via any means, so

> a naive (but possibly inefficient) solution that is quick to write
> and produces the answer is better than an optimized one that is slow
> to write

From this principle, it informs my choices on how to write Haskell
quickly for the initial solve.

- put everything in main, variables/expressions used more than once go
  in a `let` expression, use a short, random identifier unless you can
  think of a good one within 3 seconds
- for mapping over lists, use `<$>`, but if you need to filter, use
  `let` expressions or iterate over a cartesian product of lists, list
  comprehensions/monadic syntax is better
- never discard parts of input even if it makes part 1 faster to
  solve, since almost always part 2 will use that information and
  you'll have to adjust the `String -> data` step again
- avoid explicit recursion when possible, or at the least use tail
  recursion with a function named `go`
- print intermediate results as you process data, especially if the
  transformation is complex, so `do { let a = f x; print a; let b = g
  a; ... }` over `do { print (f (g x)) }`
- if using the state monad (should be a last resort), use a record for
  the state

## Best benchmarks so far
<details>
<summary>CPU details</summary>

```
Apple M1 Pro, 32 GB RAM, 10 Threads
```
</details>

### Day 1
<details>

```
benchmarking day1/part1
time                 119.3 μs   (119.2 μs .. 119.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 119.4 μs   (119.2 μs .. 119.5 μs)
std dev              532.6 ns   (424.8 ns .. 700.4 ns)

benchmarking day1/part2
time                 732.2 μs   (731.3 μs .. 733.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 732.5 μs   (731.1 μs .. 733.9 μs)
std dev              4.863 μs   (3.902 μs .. 6.437 μs)
```
</details>

### Day 2
<details>

```
benchmarking day2/part1
time                 1.049 μs   (1.042 μs .. 1.055 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.048 μs   (1.046 μs .. 1.051 μs)
std dev              8.988 ns   (7.142 ns .. 11.40 ns)

benchmarking day2/part2
time                 1.894 μs   (1.890 μs .. 1.898 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.894 μs   (1.891 μs .. 1.900 μs)
std dev              13.86 ns   (7.967 ns .. 22.19 ns)

```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 1.613 ms   (1.611 ms .. 1.617 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.635 ms   (1.630 ms .. 1.640 ms)
std dev              18.89 μs   (15.61 μs .. 23.14 μs)

benchmarking day3/part2
time                 776.8 μs   (771.6 μs .. 783.0 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 774.0 μs   (772.1 μs .. 777.1 μs)
std dev              8.316 μs   (6.077 μs .. 12.97 μs)
```
</details>

### Day 4
<details>

```
benchmarking day4/part1
time                 4.990 μs   (4.987 μs .. 4.993 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.993 μs   (4.990 μs .. 4.995 μs)
std dev              9.139 ns   (7.493 ns .. 11.07 ns)

benchmarking day4/part2
time                 28.03 μs   (28.02 μs .. 28.05 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 28.07 μs   (28.03 μs .. 28.25 μs)
std dev              240.3 ns   (25.74 ns .. 550.1 ns)
```
</details>
